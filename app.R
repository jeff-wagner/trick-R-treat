# ------------------------------------------------------------------#
# ------------------ Pumpkin Carving with R (Shiny) ----------------#
# --------------------------- Jeff Wagner --------------------------#
# -- Adapted from the BMSC Blog (https://bmscblog.wordpress.com/) --#
# ------------------------------------------------------------------#
#
# Run with:  shiny::runApp()   (from the project folder)
#
# Click on the pumpkin to drop points, then click the first point again
# (or press Enter / "Finish shape") to carve. Or switch to Stamp mode and
# click to place ready-made eyes, noses and grins.

library(shiny)
library(bslib)

# ---- Drawing helpers ------------------------------------------------

sky_col     <- "#0b0b1e"
flesh_col   <- "#2a1205" # unlit cut-out
rind_border <- "#a6460a"

ellipse <- function(cx, cy, rx, ry, n = 200) {
  t <- seq(0, 2 * pi, length.out = n)
  list(x = cx + rx * cos(t), y = cy + ry * sin(t))
}

rotate <- function(shape, cx, cy, deg) {
  a <- deg * pi / 180
  dx <- shape$x - cx
  dy <- shape$y - cy
  list(x = cx + dx * cos(a) - dy * sin(a),
       y = cy + dx * sin(a) + dy * cos(a))
}

mirror <- function(shape) list(x = -shape$x, y = shape$y)

# Ready-made shapes, centred on (cx, cy) with a nominal half-width `size`
stamp_shape <- function(type, cx, cy, size, rot = 0) {
  s <- switch(type,
    triangle = {
      a <- c(90, 210, 330) * pi / 180
      list(x = cx + size * cos(a), y = cy + size * sin(a))
    },
    circle = ellipse(cx, cy, size, size, 80),
    diamond = list(x = cx + size * c(0, 0.7, 0, -0.7),
                   y = cy + size * c(1, 0, -1, 0)),
    star = {
      a <- seq(90, 90 + 360, length.out = 11)[-11] * pi / 180
      r <- rep(c(size, size * 0.45), 5)
      list(x = cx + r * cos(a), y = cy + r * sin(a))
    },
    grin = {
      u <- seq(-1, 1, length.out = 120)
      arc <- sqrt(1 - u^2)
      list(x = cx + size * c(u, rev(u)),
           y = cy + size * c(-0.55 * arc, rev(-0.12 * arc)))
    },
    toothy = {
      u <- seq(-1, 1, length.out = 400)
      arc <- sqrt(1 - u^2)
      teeth <- (abs(u) > 0.22 & abs(u) < 0.48) * 0.2
      list(x = cx + size * c(u, rev(u)),
           y = cy + size * c(-0.55 * arc, rev(-0.12 * arc - teeth)))
    }
  )
  if (rot != 0) s <- rotate(s, cx, cy, rot)
  s
}

make_stars <- function(n = 400) {
  data.frame(x = runif(n, -1.3, 1.3),
             y = runif(n, -1, 1),
             cex = rchisq(n, 1) / 6)
}

draw_cut <- function(cut, lit, glow) {
  fill <- if (lit) glow else flesh_col
  if (isTRUE(cut$smooth)) {
    xspline(cut$x, cut$y, shape = 0.6, open = FALSE,
            col = fill, border = "#5a2a05", lwd = 3)
  } else {
    polygon(cut$x, cut$y, col = fill, border = "#5a2a05", lwd = 3)
  }
}

draw_pumpkin <- function(cuts, current = NULL, stars, lit = TRUE,
                         glow = "#ffd23f", show_moon = TRUE) {
  par(bg = sky_col, mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)

  # Night sky
  points(stars$x, stars$y, pch = 19, cex = stars$cex,
         col = adjustcolor("white", 0.8))
  if (show_moon) {
    m <- ellipse(0.78, 0.78, 0.13, 0.13)
    polygon(m, col = "#f4f1d0", border = NA)
    polygon(ellipse(0.72, 0.82, 0.12, 0.12), col = sky_col, border = NA)
  }

  # Candle glow around the pumpkin
  if (lit && length(cuts) > 0) {
    for (r in seq(1.15, 0.85, length.out = 8)) {
      polygon(ellipse(0, -0.2, r, r * 0.8),
              col = adjustcolor(glow, 0.035), border = NA)
    }
  }

  # Stem (drawn first so the lobes cover its base)
  polygon(c(-0.07, -0.06, -0.02, 0.06, 0.08, 0.06),
          c(0.36, 0.58, 0.66, 0.67, 0.62, 0.36),
          col = "#2f5d1e", border = "#1d3a12", lwd = 3)

  # Pumpkin body: outer lobes first, centre lobe on top
  lobes <- data.frame(cx   = c(-0.45, 0.45, -0.24, 0.24, 0),
                      rx   = c(0.38, 0.38, 0.42, 0.42, 0.36),
                      fill = c("#d35c0a", "#d35c0a", "#ea7012", "#ea7012", "#f7841b"))
  for (i in seq_len(nrow(lobes))) {
    polygon(ellipse(lobes$cx[i], -0.2, lobes$rx[i], 0.62),
            col = lobes$fill[i], border = rind_border, lwd = 2)
  }

  # Carvings
  for (group in cuts) for (cut in group) draw_cut(cut, lit, glow)

  # Shape in progress
  if (!is.null(current) && length(current$x) > 0) {
    lines(current$x, current$y, col = "#7fffd4", lwd = 2, lty = 2)
    points(current$x, current$y, pch = 19, col = "#7fffd4", cex = 1.2)
    points(current$x[1], current$y[1], pch = 1, col = "white", cex = 2.6, lwd = 2)
  }
}

# ---- UI -------------------------------------------------------------

stamp_choices <- c("Triangle" = "triangle", "Circle" = "circle",
                   "Diamond" = "diamond", "Star" = "star",
                   "Grin" = "grin", "Toothy grin" = "toothy")

keyboard_js <- tags$script(HTML("
document.addEventListener('keydown', function(e) {
  var tag = (e.target.tagName || '').toLowerCase();
  if (tag === 'input' || tag === 'textarea' || tag === 'select') return;
  var key = null;
  if (e.key === 'Enter') key = 'finish';
  else if (e.key === 'Escape') key = 'cancel';
  else if (e.key === 'Backspace' || (e.key === 'z' && (e.ctrlKey || e.metaKey))) key = 'undo';
  if (key) {
    e.preventDefault();
    Shiny.setInputValue('key', {key: key, t: Date.now()}, {priority: 'event'});
  }
});
"))

# Stamp preview: drawn in the browser on an SVG layer over the plot, so it
# follows the mouse without re-rendering the R plot. The server sends the
# stamp outline (centred on 0,0) and the plot's par("usr") for the mapping.
preview_js <- tags$script(HTML("
(function() {
  var tmpl = null, usr = null, mouse = null;

  function draw() {
    var svg = document.getElementById('stamp-preview');
    var img = document.querySelector('#pumpkin img');
    if (!svg) return;
    svg.innerHTML = '';
    if (!tmpl || !tmpl.active || !usr || !mouse || !img) return;

    var wrap = document.getElementById('carve-wrap').getBoundingClientRect();
    var r = img.getBoundingClientRect();
    svg.style.left = (r.left - wrap.left) + 'px';
    svg.style.top = (r.top - wrap.top) + 'px';
    svg.setAttribute('width', r.width);
    svg.setAttribute('height', r.height);

    var cx = usr[0] + (mouse.x - r.left) / r.width * (usr[1] - usr[0]);
    var cy = usr[3] - (mouse.y - r.top) / r.height * (usr[3] - usr[2]);
    var meanX = tmpl.x.reduce(function(a, b) { return a + b; }, 0) / tmpl.x.length;

    var signs = [1];
    if (tmpl.mirror && Math.abs(cx + meanX) > 0.05) signs.push(-1);
    signs.forEach(function(s) {
      var pts = tmpl.x.map(function(x, i) {
        var px = (s * (cx + x) - usr[0]) / (usr[1] - usr[0]) * r.width;
        var py = (usr[3] - (cy + tmpl.y[i])) / (usr[3] - usr[2]) * r.height;
        return px.toFixed(1) + ',' + py.toFixed(1);
      }).join(' ');
      var poly = document.createElementNS('http://www.w3.org/2000/svg', 'polygon');
      poly.setAttribute('points', pts);
      poly.setAttribute('fill', 'rgba(255, 210, 63, 0.3)');
      poly.setAttribute('stroke', '#7fffd4');
      poly.setAttribute('stroke-width', '2');
      poly.setAttribute('stroke-dasharray', '6 4');
      svg.appendChild(poly);
    });
  }

  document.addEventListener('mousemove', function(e) {
    var img = document.querySelector('#pumpkin img');
    var r = img && img.getBoundingClientRect();
    var inside = r && e.clientX >= r.left && e.clientX <= r.right &&
                 e.clientY >= r.top && e.clientY <= r.bottom;
    var next = inside ? {x: e.clientX, y: e.clientY} : null;
    if (next || mouse) { mouse = next; draw(); }
  });

  Shiny.addCustomMessageHandler('stamp_template', function(m) { tmpl = m; draw(); });
  Shiny.addCustomMessageHandler('plot_usr', function(m) { usr = m; draw(); });
})();
"))

ui <- page_sidebar(
  title = "trick-R-treat: carve a pumpkin",
  theme = bs_theme(bg = "#16111f", fg = "#f5e6d3", primary = "#ff7a18",
                   secondary = "#3a2f4d"),
  sidebar = sidebar(
    width = 300,
    radioButtons("mode", "Carving tool",
                 c("Freehand (click points)" = "free", "Stamp a shape" = "stamp")),
    conditionalPanel(
      "input.mode == 'stamp'",
      selectInput("stamp", "Shape", stamp_choices),
      sliderInput("size", "Size", min = 0.04, max = 0.45, value = 0.12, step = 0.01),
      sliderInput("rot", "Rotation", min = -180, max = 180, value = 0, step = 5)
    ),
    conditionalPanel(
      "input.mode == 'free'",
      checkboxInput("smooth", "Smooth curved edges", FALSE),
      actionButton("finish", "Finish shape (Enter)", class = "btn-primary w-100")
    ),
    input_switch("mirror", "Mirror left/right (symmetry)", TRUE),
    div(class = "d-flex gap-2",
        actionButton("undo", "Undo", class = "flex-fill btn-outline-primary"),
        actionButton("clear", "Start over", class = "flex-fill btn-outline-danger")),
    hr(),
    input_switch("lit", "Light the candle", TRUE),
    selectInput("glow", "Glow colour",
                c("Candle" = "#ffd23f", "Ember" = "#ff9a1f",
                  "Ghostly green" = "#9dff6a", "Spooky purple" = "#c58cff")),
    div(class = "d-flex gap-2",
        actionButton("sky", "New sky", class = "flex-fill btn-outline-primary"),
        downloadButton("download", "Save PNG", class = "flex-fill btn-success"))
  ),
  keyboard_js,
  card(
    full_screen = TRUE,
    card_body(
      as_fill_carrier(div(
        id = "carve-wrap", style = "position: relative;",
        plotOutput("pumpkin", height = "640px", click = "carve_click"),
        tags$svg(id = "stamp-preview",
                 style = "position: absolute; left: 0; top: 0; pointer-events: none;")
      )),
      preview_js,
      class = "p-0"
    ),
    card_footer(textOutput("status"))
  )
)

# ---- Server ---------------------------------------------------------

server <- function(input, output, session) {
  cuts    <- reactiveVal(list())          # list of groups; a group = 1-2 polygons
  current <- reactiveVal(list(x = numeric(0), y = numeric(0)))
  stars   <- reactiveVal(make_stars())

  add_group <- function(shape, smooth = FALSE) {
    shape$smooth <- smooth
    group <- list(shape)
    # Mirror unless the shape already sits on the centre line (e.g. a nose)
    if (isTRUE(input$mirror) && abs(mean(shape$x)) > 0.05) {
      m <- mirror(shape)
      m$smooth <- smooth
      group <- c(group, list(m))
    }
    cuts(c(cuts(), list(group)))
  }

  finish_shape <- function() {
    cur <- current()
    if (length(cur$x) < 3) {
      showNotification("A shape needs at least 3 points.", type = "warning")
      return()
    }
    add_group(cur, smooth = isTRUE(input$smooth))
    current(list(x = numeric(0), y = numeric(0)))
  }

  undo <- function() {
    cur <- current()
    if (length(cur$x) > 0) {
      n <- length(cur$x) - 1
      current(list(x = head(cur$x, n), y = head(cur$y, n)))
    } else if (length(cuts()) > 0) {
      cuts(head(cuts(), -1))
    }
  }

  observeEvent(input$carve_click, {
    x <- input$carve_click$x
    y <- input$carve_click$y
    if (input$mode == "stamp") {
      add_group(stamp_shape(input$stamp, x, y, input$size, input$rot))
      return()
    }
    cur <- current()
    # Clicking back near the first point closes the shape
    if (length(cur$x) >= 3 && sqrt((x - cur$x[1])^2 + (y - cur$y[1])^2) < 0.04) {
      finish_shape()
    } else {
      current(list(x = c(cur$x, x), y = c(cur$y, y)))
    }
  })

  observeEvent(input$finish, finish_shape())
  observeEvent(input$undo, undo())
  observeEvent(input$clear, {
    cuts(list())
    current(list(x = numeric(0), y = numeric(0)))
  })
  observeEvent(input$sky, stars(make_stars()))

  # Mouths need a bigger default size than eyes and noses
  observeEvent(input$stamp, {
    updateSliderInput(session, "size",
                      value = if (input$stamp %in% c("grin", "toothy")) 0.35 else 0.12)
  })
  observeEvent(input$mode, current(list(x = numeric(0), y = numeric(0))))

  observeEvent(input$key, {
    switch(input$key$key,
           finish = finish_shape(),
           cancel = current(list(x = numeric(0), y = numeric(0))),
           undo   = undo())
  })

  output$pumpkin <- renderPlot({
    draw_pumpkin(cuts(), current(), stars(), lit = input$lit, glow = input$glow)
    session$sendCustomMessage("plot_usr", par("usr"))
  }, bg = sky_col)

  # Outline for the browser-side stamp preview
  observe({
    req(input$stamp, input$size)
    s <- stamp_shape(input$stamp, 0, 0, input$size, input$rot)
    session$sendCustomMessage("stamp_template", list(
      x = s$x, y = s$y,
      mirror = isTRUE(input$mirror),
      active = identical(input$mode, "stamp")
    ))
  })

  output$status <- renderText({
    n_cur <- length(current()$x)
    n_cut <- sum(lengths(cuts()))
    if (input$mode == "stamp") {
      sprintf("Stamp mode: click to place a %s. %d cut(s) so far.",
              tolower(names(stamp_choices)[stamp_choices == input$stamp]), n_cut)
    } else if (n_cur == 0) {
      sprintf("Click on the pumpkin to start a shape. %d cut(s) so far.", n_cut)
    } else {
      sprintf("%d point(s) placed. Click the first point or press Enter to carve; Backspace undoes, Esc cancels.",
              n_cur)
    }
  })

  output$download <- downloadHandler(
    filename = function() "my-jack-o-lantern.png",
    content = function(file) {
      png(file, width = 1000, height = 1000, res = 120)
      on.exit(dev.off())
      draw_pumpkin(cuts(), NULL, stars(), lit = input$lit, glow = input$glow)
    }
  )
}

shinyApp(ui, server)
