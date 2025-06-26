#let psc-report(
  title: "title",
  crit : "crit",
  watershed : "watershed",
  corrected : "corrected",
  body,
) = {

 set text(
    font: "Open Sans",
    size: 12pt,
  )

 set page(
    "us-letter",
    margin: (left: 1in, right: 1in, top: 1.1in, bottom: 0.5in),
    background: place(top, rect(fill: rgb("5E9300"), width: 100%, height: 0.90in)),
    header: align(
      horizon,
      grid(
        columns: (75%, 25%),
        align(left, text(size: 22pt, fill: white, weight: "bold", watershed + " " + title + ": " + linebreak() + crit)),
        align(right, text(size: 12pt, fill: white, weight: "bold", 
        if corrected == "TRUE" {
            "Adjusted Acreage"
          } else {
            "Unadjusted Acreage"
          })),
      ),
    ),
    footer: align(
      grid(
        columns: (40%, 60%),
        align(horizon, context(
        text(fill: rgb("654e18"), size: 12pt, counter(page).display("1"))
        ))
      )
    )
  )

  body
}