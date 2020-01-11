module SemanticUI where

import Prelude
import Halogen (ClassName(..))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties (classes)
import Web.HTML.HTMLInputElement (indeterminate)

sdiv :: ∀ w i. Array ClassName -> Array (HTML w i) -> HTML w i
sdiv cl = HH.div [ classes cl ]

sbutton :: ∀ w i. Array ClassName -> Array (HTML w i) -> HTML w i
sbutton cl = HH.button [ classes cl ]

sicon :: ∀ w i. Array ClassName -> HTML w i
sicon cl = HH.i [ classes $ [ icon ] <> cl ] []

sa :: ∀ w i. Array ClassName -> Array (HTML w i) -> HTML w i
sa cl = HH.a [ classes cl ]

ui :: ClassName
ui = ClassName "ui"

attached :: ClassName
attached = ClassName "attached"

inverted :: ClassName
inverted = ClassName "inverted"

segment :: ClassName
segment = ClassName "segment"

secondary :: ClassName
secondary = ClassName "secondary"

pointing :: ClassName
pointing = ClassName "pointing"

menu :: ClassName
menu = ClassName "menu"

container :: ClassName
container = ClassName "container"

item :: ClassName
item = ClassName "item"

active :: ClassName
active = ClassName "active"

placeholder :: ClassName
placeholder = ClassName "placeholder"

icon :: ClassName
icon = ClassName "icon"

header :: ClassName
header = ClassName "header"

blind :: ClassName
blind = ClassName "blind"

yellow :: ClassName
yellow = ClassName "yellow"

red :: ClassName
red = ClassName "red"

green :: ClassName
green = ClassName "green"

wifi :: ClassName
wifi = ClassName "wifi"

sync :: ClassName
sync = ClassName "sync"

ban :: ClassName
ban = ClassName "ban"

statistic :: ClassName
statistic = ClassName "statistic"

statistics :: ClassName
statistics = ClassName "statistics"

value :: ClassName
value = ClassName "value"

label :: ClassName
label = ClassName "label"

three :: ClassName
three = ClassName "three"

four :: ClassName
four = ClassName "four"

table :: ClassName
table = ClassName "table"

single :: ClassName
single = ClassName "single"

line :: ClassName
line = ClassName "line"

selectable :: ClassName
selectable = ClassName "selectable"

very :: ClassName
very = ClassName "very"

compact :: ClassName
compact = ClassName "compact"

dimmer :: ClassName
dimmer = ClassName "dimmer"

loader :: ClassName
loader = ClassName "loader"

indeterminate :: ClassName
indeterminate = ClassName "indeterminate"

left :: ClassName
left = ClassName "left"

right :: ClassName
right = ClassName "right"

rail :: ClassName
rail = ClassName "rail"

toggle :: ClassName
toggle = ClassName "toggle"

checkbox :: ClassName
checkbox = ClassName "checkbox"

checked :: ClassName
checked = ClassName "checked"

basic :: ClassName
basic = ClassName "basic"

circular :: ClassName
circular = ClassName "circular"

readOnly :: ClassName
readOnly = ClassName "read-only"

segments :: ClassName
segments = ClassName "segments"

center :: ClassName
center = ClassName "center"

aligned :: ClassName
aligned = ClassName "aligned"

button :: ClassName
button = ClassName "button"

angle :: ClassName
angle = ClassName "angle"

buttons :: ClassName
buttons = ClassName "buttons"

disabled :: ClassName
disabled = ClassName "disabled"

large :: ClassName
large = ClassName "large"

labeled :: ClassName
labeled = ClassName "labeled"

pagination :: ClassName
pagination = ClassName "pagination"
