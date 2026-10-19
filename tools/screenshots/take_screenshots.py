#!/usr/bin/env python3
"""Take screenshots of the PlaViSCA app, as listed in a shot list.

Usage:
  take_screenshots.py <shot_list.yml> [--url URL] [--start-app] [--only NAME...]
                      [--out-dir DIR]

Options:
  --url URL       Address of a running app [default: http://127.0.0.1:3939]
  --start-app     Start the app with run_app.sh on the port of --url, and stop it
                  at the end. PLAVISCA_DATA_DIR must be set.
  --only NAME...  Take only the named shots.
  --out-dir DIR   Save the screenshots under DIR instead of the app directory.

The shot list (YAML) gives the browser window size, the waits, and one entry
per screenshot: the tab to open, the steps to perform on it, what to capture
and where to save it. A shot without an output is a panel of a figure: the
`figures:` section composes panels into multi-panel figures (see
compose_figures.py). See README.md, manual.yml and manuscript.yml.
Output paths are relative to the app directory (the parent of tools/), or to
--out-dir.
"""

import argparse
import os
import subprocess
import sys
import tempfile
import time
import urllib.request
from pathlib import Path

import yaml
from playwright.sync_api import sync_playwright

from PIL import Image

from compose_figures import compose_figure

APP_DIR = Path(__file__).resolve().parents[2]


def wait_for_url(url, timeout):
    """Wait until the app answers at url."""
    end = time.time() + timeout
    while time.time() < end:
        try:
            urllib.request.urlopen(url, timeout=5)
            return
        except OSError:
            time.sleep(2)
    sys.exit(f"The app did not answer at {url} within {timeout} s")


def start_app(url):
    """Start the app with run_app.sh on the host and port of url."""
    if not os.environ.get("PLAVISCA_DATA_DIR"):
        sys.exit("--start-app needs PLAVISCA_DATA_DIR")
    host_port = url.split("//", 1)[1].rstrip("/")
    host, port = host_port.split(":")
    return subprocess.Popen(
        ["./run_app.sh", "--host", host, "--port", port],
        cwd=APP_DIR, stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT,
        start_new_session=True)


def stop_app(process):
    """Stop the app started by start_app() (R runs as a child of the script)."""
    os.killpg(process.pid, 15)
    process.wait()


def run_step(page, step, settings):
    """One step of a shot; each step is a one-key mapping."""
    (action, arg), = step.items()
    if action == "drawn":
        time.sleep(settings["settle"])
        wait_until_drawn(page, settings["draw_timeout"])
    elif action == "click":
        page.click(arg)
    elif action == "wait":
        time.sleep(arg)
    elif action == "wait_for":
        page.wait_for_selector(arg, state="visible", timeout=180_000)
    elif action == "select":
        # selectize input: {id: ..., value: ...}; value may be a list
        page.evaluate(
            "([id, v]) => document.getElementById(id).selectize.setValue(v)",
            [arg["id"], arg["value"]])
    elif action == "js":
        page.evaluate(arg)
    else:
        sys.exit(f"Unknown step: {action}")


def wait_until_drawn(page, timeout):
    """Wait until the server is idle, no visible output is recalculating or
    shows a loading spinner, and every visible plotly plot is drawn.
    (Outputs on hidden tabs are paused by Shiny and stay "recalculating".)"""
    page.wait_for_function(
        """() => {
          const visible = e => e.offsetParent !== null;
          return !document.documentElement.classList.contains('shiny-busy')
            && ![...document.querySelectorAll('.recalculating')].some(visible)
            && ![...document.querySelectorAll('.load-container:not(.shiny-spinner-hidden)')].some(visible)
            && [...document.querySelectorAll('.plotly.html-widget')].filter(visible)
                 .every(e => e.querySelector('.main-svg'));
        }""",
        timeout=timeout * 1000, polling=500)


def crop_image(path, crop, scale):
    """Cut {top, bottom, left, right} CSS pixels off the edges of the image."""
    image = Image.open(path)
    cut = {side: round(crop.get(side, 0) * scale) for side in ("top", "bottom", "left", "right")}
    image.crop((cut["left"], cut["top"], image.width - cut["right"],
                image.height - cut["bottom"])).save(path)


def take_shot(page, shot, settings, out):
    """Open the shot's tab, perform its steps and save the screenshot to out."""
    if "tab" in shot:
        page.click(f"#tabset a[data-value='{shot['tab']}']")
        time.sleep(1)
    for step in shot.get("steps", []):
        run_step(page, step, settings)
    time.sleep(shot.get("settle", settings["settle"]))
    wait_until_drawn(page, settings["draw_timeout"])
    out.parent.mkdir(parents=True, exist_ok=True)
    if "element" in shot:
        page.locator(shot["element"]).first.screenshot(path=out)
    else:
        page.screenshot(path=out, full_page=shot.get("full_page", False))
    if "crop" in shot:
        crop_image(out, shot["crop"], settings["scale"])
    print(f"{shot['name']}: {out}")


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("shot_list")
    parser.add_argument("--url", default="http://127.0.0.1:3939")
    parser.add_argument("--start-app", action="store_true")
    parser.add_argument("--only", nargs="+")
    parser.add_argument("--out-dir", type=Path, default=APP_DIR)
    args = parser.parse_args()

    spec = yaml.safe_load(Path(args.shot_list).read_text())
    settings = {"width": 1500, "height": 950, "scale": 1, "settle": 2,
                "draw_timeout": 180, "start_timeout": 300}
    settings.update(spec.get("settings", {}))
    shots = spec["shots"]
    if args.only:
        shots = [s for s in shots if s["name"] in args.only]

    out_dir = args.out_dir.resolve()
    panel_dir = Path(tempfile.mkdtemp(prefix="plavisca_panels_"))
    paths = {s["name"]: out_dir / s["output"] if "output" in s else panel_dir / f"{s['name']}.png"
             for s in spec["shots"]}

    app = start_app(args.url) if args.start_app else None
    try:
        wait_for_url(args.url, settings["start_timeout"])
        with sync_playwright() as p:
            browser = p.chromium.launch()
            for shot in shots:
                # a fresh page per shot, so that shots do not depend on each other
                page = browser.new_page(
                    viewport={"width": settings["width"], "height": settings["height"]},
                    device_scale_factor=settings["scale"])
                page.goto(args.url)
                page.wait_for_selector("#tabset", timeout=settings["start_timeout"] * 1000)
                time.sleep(settings["settle"])
                take_shot(page, shot, settings, paths[shot["name"]])
                page.close()
            browser.close()
        taken = {s["name"] for s in shots}
        for figure in spec.get("figures", []):
            names = [n for row in figure["rows"] for n in row]
            if all(n in taken for n in names):
                rows = [[paths[n] for n in row] for row in figure["rows"]]
                compose_figure(rows, out_dir / figure["output"])
                print(f"{figure['name']}: {out_dir / figure['output']}")
    finally:
        if app:
            stop_app(app)
        for f in panel_dir.glob("*.png"):
            f.unlink()
        panel_dir.rmdir()


if __name__ == "__main__":
    main()
