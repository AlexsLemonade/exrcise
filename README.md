# exrcise: Selectively Excise Code Chunks for Exercises

The `exrcise` package is designed to simplify the task of maintaining separate exercise and solution notebooks for teaching with Rmarkdown notebooks.
Rather than maintaining two versions of the same file, one with answers and one without, an instructor can write a single Rmarkdown file, flagging code chunks to be removed with a chunk option such as `solution = TRUE`.
These files can then be processed with `exrcise()` to replace the code in those chunks with placeholder text, producing a new Rmarkdown file that can be distributed to students.

A usage scenario would be when one has an .Rmd file like the one below:

`car_plots.Rmd`:

~~~
---
title: "Plotting Cars"
output: html_notebook
---

Here is a simple plot of the `cars` dataset

```{r}
plot(cars)
```

Use the block below to make a version of the above plot with red points.

```{r solution = TRUE}
plot(cars, col = "red")
```
~~~

This can then be processed with `exrcise` as follows:

```r
exrcise("car_plots.Rmd", "car_plots_workbook.Rmd",
        replace_flags = "solution",
        replacement = "### Your code here")
```

to produce

`car_plots_workbook.Rmd`:

~~~
---
title: "Plotting Cars"
output: html_notebook
---

Here is a simple plot of the `cars` dataset

```{r}
plot(cars)
```

Use the block below to make a version of the above plot with red points.

```{r solution = TRUE}
### Your code here
```
~~~