#!/usr/bin/env python3

"""
Basic build-script to create HTML book from .lhs sources,
mostly using basic templating
"""

import os
import shutil
import subprocess

def read_file(filepath):
    with open(filepath, "r") as f:
        return f.read()

def open_template(name):
    return read_file(name + ".template")

def apply_template(template, substs):
    t = template
    for (k, v) in substs.items():
        t = t.replace("{" + k + "}", v)
    return t

def write_string_to_file(s, filepath):
    with open(filepath, "w") as f:
        f.write(s)

def render_lhs(lhs_filepath):
    return subprocess.check_output([
        "pandoc", lhs_filepath,
                  "-f", "markdown+lhs",
                  "-t", "html",
                  "--mathjax"
    ]).decode("utf8")

def has_image_ext(filename):
    _, ext = os.path.splitext(filename)
    return ext in [".png", ".jpg", ".jpeg", ".gif", ".svg"]

def copy_images(source_dir, dest_dir):
    images = [path for path in os.listdir(source_dir) if has_image_ext(path)]
    for image in images:
        shutil.copy(os.path.join(source_dir, image), dest_dir)

def build_sections(sources):
    chapter_templ = open_template("chapter")
    prev_chap_href = "../index.html"
    prev_chap_name = "Table of contents"
    chapters = [(s, c_n, c_s) for (s, cs) in sources for (c_n, c_s) in cs]
    for i in range(0, len(chapters)):
        (section, chapter_name, chapter_source) = chapters[i]
        next_chap_href = "../index.html"
        next_chap_name = "Table of contents"
        if (i + 1) < len(chapters):
            next_section, next_chap_name, _ = chapters[i + 1]
            next_chap_href = "../{}/{}.html".format(next_section, next_chap_name)
        if not os.path.exists(section):
            os.makedirs(section)
        chapter_path = "../../" + chapter_source
        content = render_lhs(chapter_path)
        chapter = apply_template(
            chapter_templ,
            {
                "content": content,
                "previous-href": prev_chap_href,
                "previous-name": prev_chap_name,
                "next-href": next_chap_href,
                "next-name": next_chap_name,
            })
        out_path = "{}/{}.html".format(section, chapter_name)
        write_string_to_file(chapter, out_path)
        copy_images(os.path.dirname(chapter_path), section)
        prev_chap_href = "../" + out_path
        prev_chap_name = chapter_name

def build_index(sources):
    toc = "<ol>\n"
    for (section_name, chapter_sources) in sources:
        toc += "<li>\n"
        toc += "<div>" + section_name + "</div>\n"
        toc += "<ul>\n"
        for (chapter_name, _) in chapter_sources:
            chapter_path = "{}/{}.html".format(section_name, chapter_name)
            toc += "<li><a href=\"{}\">{}</a></li>\n".format(chapter_path, chapter_name)
        toc += "</ul>\n</li>\n"
    index_template = open_template("index")
    index = apply_template(index_template, { "toc": toc })
    write_string_to_file(index, "index.html")

sources = [
    ("Dimensions", [
        ("Introduction", "Physics/src/Dimensions/Intro.lhs"),
        ("Value-level dimensions", "Physics/src/Dimensions/ValueLevel.lhs"),
        ("Type-level dimensions", "Physics/src/Dimensions/TypeLevel.lhs"),
        ("Quantities", "Physics/src/Dimensions/Quantity.lhs"),
        ("Usage", "Physics/src/Dimensions/Usage.lhs"),
    ]),
    ("Vectors", [
        ("Vector", "Physics/src/Vector/Vector.lhs")
    ]),
    ("Calculus", [
        ("Calculus", "Physics/src/Calculus/Calculus.lhs"),
    ])
]

if not os.path.exists("build"):
    os.makedirs("build")

shutil.copy("style.css", "build")
shutil.copy("index.template", "build")
shutil.copy("chapter.template", "build")

os.chdir("build")

build_sections(sources)
build_index(sources)

os.remove("index.template")
os.remove("chapter.template")
