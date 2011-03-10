import re
from bisect import bisect



from pygments.lexers.agile import PythonLexer
from pygments.lexer import Lexer, LexerContext, RegexLexer, ExtendedRegexLexer, \
     bygroups, include, using, this, do_insertions
from pygments.token import Punctuation, Text, Comment, Keyword, Name, String, \
     Generic, Operator, Number, Whitespace, Literal
from pygments.util import get_bool_opt
from pygments.lexers.other import BashLexer

from pygments.lexers.math import SLexer
from pygments.lexers.text import TexLexer


class RnwLexer(RegexLexer):
    name = "Sweave"
    aliases = ["rnw", 'Rnw', 'RNW', 'snw', 'Snw', 'SNW']
    filenames = ['*.Rnw', '*.rnw', '*.Snw', '*.snw']

    tokens = {
        'general': [
            (r'%.*?\n', Comment),
            (r'[{}]', Name.Builtin),
            (r'[&_^]', Name.Builtin),
        ],
        'rcode': [
            (r'(.+)\n@\n', bygroups(using(SLexer), Name.Variable), "#pop")
        ],
        'root': [
            (r'<<.*>>=', Name.Variable, 'rcode'),
            (r'\\\[', String.Backtick, 'displaymath'),
            (r'\\\(', String, 'inlinemath'),
            (r'\$\$', String.Backtick, 'displaymath'),
            (r'\$', String, 'inlinemath'),
            (r'\\([a-zA-Z]+|.)', Keyword, 'command'),
            include('general'),
            (r'[^\\$%&_^{}]+', Text),
        ],
        'math': [
            (r'\\([a-zA-Z]+|.)', Name.Variable),
            include('general'),
            (r'[0-9]+', Number),
            (r'[-=!+*/()\[\]]', Operator),
            (r'[^=!+*/()\[\]\\$%&_^{}0-9-]+', Name.Builtin),
        ],
        'inlinemath': [
            (r'\\\)', String, '#pop'),
            (r'\$', String, '#pop'),
            include('math'),
        ],
        'displaymath': [
            (r'\\\]', String, '#pop'),
            (r'\$\$', String, '#pop'),
            (r'\$', Name.Builtin),
            include('math'),
        ],
        'command': [
            (r'\[.*?\]', Name.Attribute),
            (r'\*', Keyword),
            (r'', Text, '#pop'),
        ],
    }

    def analyse_text(text):
        for start in ("\\documentclass", "\\input", "\\documentstyle",
                      "\\relax"):
            if text[:len(start)] == start:
                return True



# class TexLexer(RegexLexer):
#     """
#     Lexer for the TeX and LaTeX typesetting languages.
#     """
# 
#     name = 'TeX'
#     aliases = ['tex', 'latex']
#     filenames = ['*.tex', '*.aux', '*.toc']
#     mimetypes = ['text/x-tex', 'text/x-latex']
# 
#     tokens = {
#         'general': [
#             (r'%.*?\n', Comment),
#             (r'[{}]', Name.Builtin),
#             (r'[&_^]', Name.Builtin),
#         ],
#         'root': [
#             (r'\\\[', String.Backtick, 'displaymath'),
#             (r'\\\(', String, 'inlinemath'),
#             (r'\$\$', String.Backtick, 'displaymath'),
#             (r'\$', String, 'inlinemath'),
#             (r'\\([a-zA-Z]+|.)', Keyword, 'command'),
#             include('general'),
#             (r'[^\\$%&_^{}]+', Text),
#         ],
#         'math': [
#             (r'\\([a-zA-Z]+|.)', Name.Variable),
#             include('general'),
#             (r'[0-9]+', Number),
#             (r'[-=!+*/()\[\]]', Operator),
#             (r'[^=!+*/()\[\]\\$%&_^{}0-9-]+', Name.Builtin),
#         ],
#         'inlinemath': [
#             (r'\\\)', String, '#pop'),
#             (r'\$', String, '#pop'),
#             include('math'),
#         ],
#         'displaymath': [
#             (r'\\\]', String, '#pop'),
#             (r'\$\$', String, '#pop'),
#             (r'\$', Name.Builtin),
#             include('math'),
#         ],
#         'command': [
#             (r'\[.*?\]', Name.Attribute),
#             (r'\*', Keyword),
#             (r'', Text, '#pop'),
#         ],
#     }
# 
#     def analyse_text(text):
#         for start in ("\\documentclass", "\\input", "\\documentstyle",
#                       "\\relax"):
#             if text[:len(start)] == start:
#                 return True
# 
# 
# class SLexer(RegexLexer):
#     """
#     For S, S-plus, and R source code.
# 
#     *New in Pygments 0.10.*
#     """
# 
#     name = 'S'
#     aliases = ['splus', 's', 'r']
#     filenames = ['*.S', '*.R']
#     mimetypes = ['text/S-plus', 'text/S', 'text/R']
# 
#     tokens = {
#         'comments': [
#             (r'#.*$', Comment.Single),
#         ],
#         'valid_name': [
#             (r'[a-zA-Z][0-9a-zA-Z\._]+', Text),
#             (r'`.+`', String.Backtick),
#         ],
#         'punctuation': [
#             (r'\[|\]|\[\[|\]\]|\$|\(|\)|@|:::?|;|,', Punctuation),
#         ],
#         'keywords': [
#             (r'for(?=\s*\()|while(?=\s*\()|if(?=\s*\()|(?<=\s)else|'
#              r'(?<=\s)break(?=;|$)|return(?=\s*\()|function(?=\s*\()',
#              Keyword.Reserved)
#         ],
#         'operators': [
#             (r'<-|-|==|<=|>=|<|>|&&|&|!=|\|\|?', Operator),
#             (r'\*|\+|\^|/|%%|%/%|=', Operator),
#             (r'%in%|%*%', Operator)
#         ],
#         'builtin_symbols': [
#             (r'(NULL|NA|TRUE|FALSE|NaN)\b', Keyword.Constant),
#             (r'(T|F)\b', Keyword.Variable),
#         ],
#         'numbers': [
#             (r'(?<![0-9a-zA-Z\)\}\]`\"])(?=\s*)[-\+]?[0-9]+'
#              r'(\.[0-9]*)?(E[0-9][-\+]?(\.[0-9]*)?)?', Number),
#             (r'\.[0-9]*(E[0-9][-\+]?(\.[0-9]*)?)?', Number),
#         ],
#         'statements': [
#             include('comments'),
#             # whitespaces
#             (r'\s+', Text),
#             (r'\'', String, 'string_squote'),
#             (r'\"', String, 'string_dquote'),
#             include('builtin_symbols'),
#             include('numbers'),
#             include('keywords'),
#             include('punctuation'),
#             include('operators'),
#             include('valid_name'),
#         ],
#         'root': [
#             include('statements'),
#             # blocks:
#             (r'\{|\}', Punctuation),
#             #(r'\{', Punctuation, 'block'),
#             (r'.', Text),
#         ],
#         #'block': [
#         #    include('statements'),
#         #    ('\{', Punctuation, '#push'),
#         #    ('\}', Punctuation, '#pop')
#         #],
#         'string_squote': [
#             (r'[^\']*\'', String, '#pop'),
#         ],
#         'string_dquote': [
#             (r'[^\"]*\"', String, '#pop'),
#         ],
#     }
# 
#     def analyse_text(text):
#         return '<-' in text
