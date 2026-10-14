# trick-**R**-treat

## Pumpkin Carving with R by Jeff Wagner 
## October 20, 2020 
### Adapted from the BMSC Blog (https://bmscblog.wordpress.com/) 


Carve your very own digital pumpkin with R!

![Shiny app screenshot](Examples/shiny-app.png)

### Shiny app (recommended)

```r
install.packages(c("shiny", "bslib"))  # first time only
shiny::runApp()                        # from the project folder
```

* **Freehand:** click on the pumpkin to drop points. Close the shape by clicking the first point again, or by pressing **Enter**/**Finish shape**. Tick *Smooth curved edges* for rounder cuts.
* **Stamp a shape:** a dashed outline follows your mouse to show where the shape will go, mirrored copy included. Click to place ready-made triangles, circles, diamonds, stars, grins or toothy grins. Use the sliders to change size and rotation.
* **Mirror left/right:** carve one eye and the other appears automatically. Shapes placed on the centre line, like a nose, are not doubled.
* **Undo** (or Backspace) removes the last point or cut. **Esc** cancels the shape in progress. **Start over** clears everything.
* Light or snuff out the candle, pick a glow colour, roll a **New sky**, and **Save PNG** to keep your jack-o'-lantern.

### Deploying to Posit Connect Cloud

`manifest.json` lists the app file (`app.R`) and its R package dependencies, so Connect Cloud can deploy it straight from this GitHub repo. In Connect Cloud, choose **Publish → Shiny**, pick this repository and branch, and select `app.R` as the primary file.

If you change the app's packages, regenerate the manifest from the project folder:

```r
rsconnect::writeManifest(appFiles = "app.R")
```

### Classic script

`RpumpkinCarving.R` is the original version, which uses `locator()`. Change the number of points in each `locator()` call to make simpler or more complex carvings.

Have fun and stay spooky!
