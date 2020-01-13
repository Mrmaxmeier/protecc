module SemanticUI where

import Prelude
import Halogen (ClassName(..))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties (classes)
import Partial.Unsafe (unsafeCrashWith)
import Util (mwhen)

sdiv :: ∀ w i. Array ClassName -> Array (HTML w i) -> HTML w i
sdiv cl = HH.div [ classes cl ]

sbutton :: ∀ w i. Array ClassName -> Array (HTML w i) -> HTML w i
sbutton cl = HH.button [ classes cl ]

sicon :: ∀ w i. Array ClassName -> HTML w i
sicon cl = HH.i [ classes $ [ icon ] <> cl ] []

sa :: ∀ w i. Array ClassName -> Array (HTML w i) -> HTML w i
sa cl = HH.a [ classes cl ]

width :: Int -> ClassName
width = case _ of
  1 -> one
  2 -> two
  3 -> three
  4 -> four
  5 -> five
  6 -> six
  7 -> seven
  8 -> eight
  9 -> nine
  10 -> ten
  11 -> eleven
  12 -> twelve
  13 -> thirteen
  14 -> fourteen
  15 -> fifteen
  16 -> sixteen
  i -> unsafeCrashWith $ "No width class exists for " <> show i

loaderDiv :: ∀ b w i. HeytingAlgebra b => Eq b => b -> HTML w i
loaderDiv indet =
  sdiv [ ui, placeholder, segment ]
    [ sdiv [ ui, active, dimmer ]
        [ sdiv ([ ui, loader ] <> mwhen indet [ indeterminate ]) []
        ]
    ]

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

one :: ClassName
one = ClassName "one"

two :: ClassName
two = ClassName "two"

three :: ClassName
three = ClassName "three"

four :: ClassName
four = ClassName "four"

five :: ClassName
five = ClassName "five"

six :: ClassName
six = ClassName "six"

seven :: ClassName
seven = ClassName "seven"

eight :: ClassName
eight = ClassName "eight"

nine :: ClassName
nine = ClassName "nine"

ten :: ClassName
ten = ClassName "ten"

eleven :: ClassName
eleven = ClassName "eleven"

twelve :: ClassName
twelve = ClassName "twelve"

thirteen :: ClassName
thirteen = ClassName "thirteen"

fourteen :: ClassName
fourteen = ClassName "fourteen"

fifteen :: ClassName
fifteen = ClassName "fifteen"

sixteen :: ClassName
sixteen = ClassName "sixteen"

wide :: ClassName
wide = ClassName "wide"

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

notched :: ClassName
notched = ClassName "notched"

circle :: ClassName
circle = ClassName "circle"

loading :: ClassName
loading = ClassName "loading"

modal :: ClassName
modal = ClassName "modal"

fullscreen :: ClassName
fullscreen = ClassName "fullscreen"

visible :: ClassName
visible = ClassName "visible"

modals :: ClassName
modals = ClassName "modals"

page :: ClassName
page = ClassName "page"

transition :: ClassName
transition = ClassName "transition"

content :: ClassName
content = ClassName "content"

celled :: ClassName
celled = ClassName "celled"

divider :: ClassName
divider = ClassName "divider"

message :: ClassName
message = ClassName "message"

grey :: ClassName
grey = ClassName "grey"

blue :: ClassName
blue = ClassName "blue"

purple :: ClassName
purple = ClassName "purple"

yellow :: ClassName
yellow = ClassName "yellow"

red :: ClassName
red = ClassName "red"

detail :: ClassName
detail = ClassName "detail"

scrolling :: ClassName
scrolling = ClassName "scrolling"

wrap :: ClassName
wrap = ClassName "wrap"

scroll :: ClassName
scroll = ClassName "scroll"

selection :: ClassName
selection = ClassName "selection"

dropdown :: ClassName
dropdown = ClassName "dropdown"

collapsing :: ClassName
collapsing = ClassName "collapsing"

mini :: ClassName
mini = ClassName "mini"

field :: ClassName
field = ClassName "field"

form :: ClassName
form = ClassName "form"

simple :: ClassName
simple = ClassName "simple"
