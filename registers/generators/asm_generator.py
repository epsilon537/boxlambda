
# -*- coding: utf-8 -*-

""" Create custom generator
"""
from corsair import RegisterMap, generators, config, __version__, utils


class AsmGenerator(generators.Generator, generators.Jinja2):
    """Create assembler .s file with register map defines.

    :param rmap: Register map object
    :type rmap: :class:`corsair.RegisterMap`
    :param path: Path to the output file
    :type path: str
    :param prefix: Prefix for the all defines and types. If empty output file name will be used.
    :type prefix: str
    """

    def __init__(self, rmap=None, path='regs.h', prefix="CSR", **args):
        super().__init__(rmap, **args)
        self.path = path
        self.prefix = prefix

    def validate(self):
        super().validate()
        data_width_allowed = [8, 16, 32, 64]
        assert config.globcfg['data_width'] in [8, 16, 32, 64], \
            "For %s generator, global 'data_width' must be one of '%s', but current is %d" % \
            (self._name(), data_width_allowed, config.globcfg['data_width'])

    def generate(self):
        # validate parameters
        self.validate()
        # prepare jinja2
        j2_template = 'regmap_asm.j2'
        j2_vars = {}
        j2_vars['corsair_ver'] = __version__
        j2_vars['rmap'] = self.rmap
        j2_vars['prefix'] = self.prefix.upper()
        j2_vars['file_name'] = utils.get_file_name(self.path)
        j2_vars['config'] = config.globcfg
        # render
        self.render_to_file(j2_template, j2_vars, self.path, "./generators")
