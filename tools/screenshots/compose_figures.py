"""Compose screenshots into multi-panel figures (the `figures:` section of a
shot list).

Each figure is a list of rows, each row a list of shot names. The shots of a
row are scaled to the same height and the rows to the same width, stacked top
to bottom, and labelled A, B, C, ... in reading order.
"""

from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

FONT_FILES = ["DejaVuSans-Bold.ttf", "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf"]


def label_font(size):
    for name in FONT_FILES:
        try:
            return ImageFont.truetype(name, size)
        except OSError:
            pass
    return ImageFont.load_default(size=size)


def scale_to(image, width=None, height=None):
    """image scaled to the given width or height, keeping its proportions."""
    if width is not None:
        height = round(image.height * width / image.width)
    else:
        width = round(image.width * height / image.height)
    return image.resize((width, height), Image.LANCZOS)


def compose_row(images, gap):
    """images side by side at the height of the tallest one, with `gap`
    pixels between them; returns the row and the x offset of each image."""
    height = max(im.height for im in images)
    images = [scale_to(im, height=height) for im in images]
    width = sum(im.width for im in images) + gap * (len(images) - 1)
    row = Image.new("RGB", (width, height), "white")
    offsets, x = [], 0
    for im in images:
        row.paste(im, (x, 0))
        offsets.append(x)
        x += im.width + gap
    return row, offsets, images


def compose_figure(rows, out, gap=60, label_size=90, margin=110):
    """Save the figure made of `rows` (lists of image paths) to `out`. Each
    panel gets a white band of `margin` pixels above it with its letter."""
    panels = [[Image.open(p).convert("RGB") for p in row] for row in rows]
    built = [compose_row(row, gap) for row in panels]
    width = max(row.width for row, _, _ in built)
    scaled = []
    for row, offsets, images in built:
        factor = width / row.width
        scaled.append((scale_to(row, width=width), [round(x * factor) for x in offsets]))

    height = sum(margin + row.height for row, _ in scaled) + gap * (len(scaled) - 1)
    figure = Image.new("RGB", (width, height), "white")
    draw = ImageDraw.Draw(figure)
    font = label_font(label_size)
    y, letter = 0, ord("A")
    for row, offsets in scaled:
        for x in offsets:
            draw.text((x, y), chr(letter), fill="black", font=font)
            letter += 1
        figure.paste(row, (0, y + margin))
        y += margin + row.height + gap
    Path(out).parent.mkdir(parents=True, exist_ok=True)
    figure.save(out, dpi=(600, 600))
