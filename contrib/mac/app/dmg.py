from __future__ import unicode_literals

import os.path

appname = defines['app']

files = [ os.path.basename(appname) ]
symlinks = { 'Applications': '/Applications' }

format = "UDZO"
size = "500m"

badge_icon = "julia.icns"

window_rect = ((400,100),(300,300))

background = "background.pdf"
icon_size = 64
icon_locations = {
    appname: (80, 170),
    'Applications': (248, 170)
}
