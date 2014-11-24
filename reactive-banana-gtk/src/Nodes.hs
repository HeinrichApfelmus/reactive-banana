import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.GTK

main = do
  initGUI
  dia <- dialogNew
  dialogAddButton dia stockOk ResponseOk
  contain <- dialogGetUpper dia
  canvas <- drawingAreaNew
  canvas `on` sizeRequest $ return (Requisition 40 40)
  ctxt <- cairoCreateContext Nothing
  text <- layoutEmpty ctxt
  text `layoutSetText` "Hello World."
  canvas `on` exposeEvent $ updateCanvas text
  boxPackStartDefaults contain canvas
  widgetShow canvas
  dialogRun dia
  return ()

updateCanvas :: PangoLayout -> EventM EExpose Bool
updateCanvas text = do
  win <- eventWindow
  (width',height') <- liftIO $ drawableGetSize win
  let width  = realToFrac width'
      height = realToFrac height'

  -- Draw using the cairo api
  liftIO $ renderWithDrawable win $ do
    setSourceRGB 1 0 0
    setLineWidth 20
    setLineCap LineCapRound
    setLineJoin LineJoinRound

    moveTo 30 30
    lineTo (width-30) (height-30)
    lineTo (width-30) 30
    lineTo 30 (height-30)
    stroke

    setSourceRGB 1 1 0
    setLineWidth 4

    save
    translate (width / 2) (height / 2)
    scale (width / 2) (height / 2)
    arc 0 0 1 (135 * pi/180) (225 * pi/180)
    restore
    stroke

    setSourceRGB 0 0 0
    moveTo 30 (realToFrac height / 4)
    rotate (pi/4)
    showLayout text


  return True

