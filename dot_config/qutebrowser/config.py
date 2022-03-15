from qutebrowser.config.configfiles import ConfigAPI
from qutebrowser.config.config import ConfigContainer

config.load_autoconfig(False)

# defaults-------------------------------------------------------------------------
config.set("content.images", True, "chrome-devtools://*")

config.set("content.images", True, "devtools://*")

# javascript settings---------------------------------------------------------
config.set("content.javascript.enabled", True, "chrome-devtools://*")

config.set("content.javascript.enabled", True, "devtools://*")

config.set("content.javascript.enabled", True, "chrome://*/*")

config.set("content.javascript.enabled", True, "qute://*/*")

config.set(
    "content.javascript.enabled",
    True,
    "file:///home/ethan/.config/qutebrowser/homepage/homepage.html",
)

config.set("content.javascript.enabled", True,
           "https://start.duckduckgo.com/*")

config.set("content.javascript.enabled", True,
           "https://www.electionbettingodds.com/*")

config.set("content.javascript.enabled", True, "https://www.invidio.us/*")
config.set("content.javascript.enabled", True, "https://www.metaculus.com/*")

config.set("content.javascript.enabled", True, "https://www.old.reddit.com/*")

config.set("content.javascript.enabled", True, "https://www.github.com/*")

config.set("content.javascript.enabled", True, "https://www.gitlab.com/*")
config.set("content.javascript.enabled", True, "https://www.lesswrong.com/*")

config.set("content.javascript.enabled", True, "https://www.keyhero.com/*")

config.set("content.javascript.enabled", True, "https://www.youtube.com/*")
config.set("content.javascript.enabled", True, "https://www.youtube.com/*")
config.set("content.javascript.enabled", True, "https://*.rust-lang.org/*")

config.set("content.javascript.enabled", True,
           "https://www.wolframalpha.com/*")
config.set("content.javascript.enabled", True,
           "https://www.search.nixos.org/*")

config.set("content.javascript.enabled", False)

config.set("content.notifications.enabled", False)

# config.set("colors.webpage.darkmode.enabled", True)


# ----------------------------------------------------------------------------------

# privacy settings

c.content.webrtc_ip_handling_policy = "default-public-interface-only"
c.content.geolocation = False
c.content.webgl = False
c.content.cookies.accept = "no-3rdparty"
c.content.canvas_reading = False
c.completion.web_history.max_items = 20
# c.content.proxy = 'socks://localhost:9050/'

# -----------------

# Misc.----------------------------
c.editor.command = ["emacsclient", "-c", "{file}"]

c.downloads.location.directory = "~/downloads/"
# ----------------------------------

# URL settings---------------------------------------------------------------------
c.url.default_page = "~/.config/qutebrowser/homepage/homepage.html"

c.url.open_base_url = True

c.url.searchengines = {
    "DEFAULT":
    # "https://start.duckduckgo.com/?q={}&kae=-1&kao=-1&kaq=-1&ko=1&kax=-1&kv=-1&kau=-1&kad=en_US&k5=2&ks=m&k1=-1&kaj=u&kay=b&kak=-1&kat=-1&kav=1&kap=-1&kbc=1&k8=e5e9f0&kaa=81a1c1&kah=wt-wt&k9=eceff4&k21=4c566a&kj=2E3440&k7=2e3440&kx=e5e9f0&kl=wt-wt",
    "https://start.duckduckgo.com/?q={}&kae=d&kao=-1&kaq=-1&ko=1&kax=-1&kv=-1&kau=-1&kad=en_US&k5=2&ks=m&k1=-1&kaj=u&kay=b&kak=-1&kat=-1&kav=1&kap=-1&kp=-2&kal=-1",
    # "DEFAULT": 'https://3g2upl4pq6kufc4m.onion/?q={}',
    "yt":
    "https://invidious.site/search?q={}",
    "wa":
    "https://www.wolframalpha.com/input/?i={}",
    "aw":
    "https://wiki.archlinux.org/index.php?search={}&title=Special%3ASearch&go=Go",
    "gh":
    "https://github.com/search?q={}",
    "wp":
    "https://en.wikipedia.org/w/index.php?search={}",
    "rd":
    "https://old.reddit.com/search?q={}",
    "om":
    "https://www.openstreetmap.org/search?query={}#map=11/40.6971/-73.9796",
    "go":
    "https://www.google.com/search?q={}",
    "rs":
    "https://doc.rust-lang.org/std/index.html?search={}",
    "me":
    "https://www.metaculus.com/questions/?order_by=-rank&search={}&categories=",
    "lg":
    "http://libgen.rs/search.php?req={}&lg_topic=libgen&open=0&view=simple&res=25&phrase=1&column=def"
}

c.url.start_pages = c.url.default_page
# ----------------------------------------------------------------------------------

# Bindings for normal mode---------------------------
config.bind(" u", "spawn --userscript urlopen.sh")
config.bind(" U", "hint links userscript urlopen.sh")

config.bind(" j", "set content.javascript.enabled true")
# ----------------------------------------------------

c.fonts.default_family = "Source Code Pro"
c.fonts.default_size = "10pt"

# theme-------------
config.source("dracula.py")
