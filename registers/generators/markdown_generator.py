# -*- coding: utf-8 -*-

"""Create custom generator"""

from corsair import RegisterMap, generators, config, __version__, utils
from pathlib import Path
import wavedrom
import json
import pdb


class Wavedrom:
    """Basic class for rendering register images with wavedrom"""

    def draw_regs(self, imgdir, rmap):
        imgdir.mkdir(exist_ok=True)

        bits = config.globcfg["data_width"]
        lanes = bits // 16 if bits > 16 else 1
        for reg in rmap:
            reg_wd = {
                "reg": [],
                "config": {"bits": bits, "lanes": lanes, "fontsize": 10},
            }
            bit_pos = -1
            for bf in reg:
                if bit_pos == -1 and bf.lsb > 0:
                    reg_wd["reg"].append({"bits": bf.lsb})
                elif bf.lsb - bit_pos > 1:
                    reg_wd["reg"].append({"bits": bf.lsb - bit_pos - 1})
                name = bf.name
                name_max_len = 5 * bf.width
                if len(bf.name) > name_max_len:  # to prevent labels overlapping
                    name = bf.name[: name_max_len - 1] + ".."
                reg_wd["reg"].append({"name": name, "attr": bf.access, "bits": bf.width})
                bit_pos = bf.msb
            if (bits - 1) > bit_pos:
                reg_wd["reg"].append({"bits": bits - bit_pos - 1})
            wavedrom.render(json.dumps(reg_wd)).saveas(str(imgdir / ("%s.svg" % reg.name.lower())))


class Markdown(generators.Generator, generators.Jinja2, Wavedrom):
    """Create documentation for a register map in Markdown.

    :param rmap: Register map object
    :type rmap: :class:`corsair.RegisterMap`
    :param path: Path to the output file
    :type path: str
    :param title: Document title
    :type title: str
    :param print_images: Enable generating images for bit fields of a register
    :type print_images: bool
    :param image_dir: Path to directory where all images will be saved
    :type image_dir: str
    :param print_conventions: Enable generating table with register access modes explained
    :type print_conventions: bool
    """

    def __init__(
        self,
        rmap=None,
        path="regs.md",
        title="Register map",
        print_images=True,
        image_dir="regs_img",
        print_conventions=True,
        **args,
    ):
        super().__init__(rmap, **args)
        self.path = path
        self.title = title
        self.print_images = print_images
        self.image_dir = image_dir
        self.print_conventions = print_conventions

    def generate(self):
        filename = utils.get_file_name(self.path)
        # validate parameters
        self.validate()
        # prepare jinja2
        j2_template = "regmap_md.j2"
        j2_vars = {}
        j2_vars["corsair_ver"] = __version__
        j2_vars["rmap"] = self.rmap
        j2_vars["print_images"] = utils.str2bool(self.print_images)
        j2_vars["print_conventions"] = utils.str2bool(self.print_conventions)
        j2_vars["image_dir"] = self.image_dir
        j2_vars["filename"] = filename
        j2_vars["title"] = self.title
        j2_vars["config"] = config.globcfg
        # render
        self.render_to_file(j2_template, j2_vars, self.path)
        # draw register images
        if self.print_images:
            self.draw_regs(Path(self.path).parent / self.image_dir, self.rmap)
