# Screenshots of the app

`take_screenshots.py` opens the app in a headless browser (Chromium, driven
by [Playwright](https://playwright.dev/python/)), performs the steps listed in
a shot list and saves one PNG per shot. Use it to refresh the images of the
user manual, or to make figures of the interface.

## Setup (once)

```bash
cd PlaViSca-app
python3 -m venv tools/screenshots/.venv
tools/screenshots/.venv/bin/pip install -r tools/screenshots/requirements.txt
tools/screenshots/.venv/bin/playwright install chromium-headless-shell
```

## Run

The script can start and stop the app itself (`--start-app`; `R` must be on
the `PATH` and `PLAVISCA_DATA_DIR` set):

```bash
PLAVISCA_DATA_DIR=/path/to/app_data \
  tools/screenshots/.venv/bin/python tools/screenshots/take_screenshots.py \
  tools/screenshots/manual.yml --start-app
```

or use an app that is already running (`--url`, default
`http://127.0.0.1:3939`). `--only embeddings filters` takes only the named
shots, and `--out-dir DIR` saves them under DIR instead of the app
directory.

Shot lists: `manual.yml` (images of the user manual) and `manuscript.yml`
(figures of the manuscript, at print resolution):

```bash
PLAVISCA_DATA_DIR=/path/to/app_data \
  tools/screenshots/.venv/bin/python tools/screenshots/take_screenshots.py \
  tools/screenshots/manuscript.yml --start-app --out-dir /path/to/manuscript/figures
``` The manual set (`manual.yml`) takes about five minutes, mostly waiting
for the 3D embeddings to draw. After refreshing the manual images, rebuild the
manual (see "The user manual" in `docs/developer.md`).

## Shot lists

A shot list is a YAML file with the window settings and one entry per
screenshot:

```yaml
settings:
  width: 1500      # browser window, in CSS pixels
  height: 950
  scale: 1         # 2 or 3 for sharper images (e.g. for print)
  settle: 3        # seconds to wait after the steps

shots:
  - name: embeddings_compare
    tab: Embeddings                      # tab to open (its title)
    steps:
      - click: "#enable_comparison"      # click an element (CSS selector)
      - drawn: true                      # wait until the plots are drawn
      - select: {id: color_second, value: study_label}   # set a dropdown
    output: manual/docs/source/imgs/embeddings_compare.png
```

Other steps: `wait: <seconds>`, `wait_for: <selector>` (until visible) and
`js: <JavaScript>`. By default the whole window is captured; `element:
<selector>` captures one element only (e.g. a card), and `full_page: true` the
whole scrollable page. Output paths are relative to the app directory. Each
shot starts from a freshly loaded app, and is saved only once the server is
idle, no output is recalculating and every visible plot is drawn.
