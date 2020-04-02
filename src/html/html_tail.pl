:- module(html_tail, [load_html_tail/1]).

load_html_tail('\n <script>\n $(function(){\n        $(".tree").treemenu({delay:100}).openActive();\n    });\n </script>\n \n </body>\n </html>').