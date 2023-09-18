# Trackman Baseball Visualizer
This is a shiny dashboard for visualizing trackman baseball data. It contains 15 types of graphs and 2 predictive models. 

It is easy and intuitive, transform your trackman data and take your analysis game to the next level!
<br/><br/>
<p align="center">
<img width="147" alt="image" src="https://github.com/byw-5/Trackman_baseball_visualizer/assets/112497612/b0041003-062d-4608-bcf9-b3391bd5db7e">
<img width="147" alt="image" src="https://github.com/byw-5/Trackman_baseball_visualizer/assets/112497612/0950973e-69c6-48c6-aaea-bac596e9b2c7">
<img width="147" alt="image" src="https://github.com/byw-5/Trackman_baseball_visualizer/assets/112497612/8eaa03db-2c40-4b72-9acb-887c6660265f">
<img width="147" alt="image" src="https://github.com/byw-5/Trackman_baseball_visualizer/assets/112497612/16e17c4a-11cf-47ca-9fe5-1ac29114fb23">
<img width="147" alt="image" src="https://github.com/byw-5/Trackman_baseball_visualizer/assets/112497612/a5bf2786-ebe9-44d7-89c7-994bf583885b">
<img width="147" alt="image" src="https://github.com/byw-5/Trackman_baseball_visualizer/assets/112497612/6af16352-8144-4c46-a6f2-de2da7fa7969">
<p align="center">
<em>Some examples of the visualizations that can be found in the dashboard</em>

## Must-knows

### 1. IMPORTANT

- Process raw trackman baseball data with "Add_necessary_columns.R".
- Plotting estimated strike zones for umpires requires pitch-level data with umpire name. Ignore this tab if the user does not have access.

### 2. Pitch Type Abbreviation

| Pitch Type      | Abbreviation |
| ----------- | ----------- |
| Four-Seamer      | FF       |
| Sinker, Two-Seamer   | ST        |
| Cutter   | CT        |
| Slider   | SL        |
| Curveball   | CR        |
| Changeup   | CH        |
| Splitter   | SP        |

### 3. Tips

- Everytime the filters in the sidebar have been modified, be sure to hit the "Apply filter" to see the corresponding changes on the graphs. As for filters in the dashboard boxes, the changes will be applied automatically.
- For every multiple-choice-possible filters, after all choices are unselected, they will all be automatically selected again.
- Use "left", "right" and "backspace" keys when using multiple select boxes.

### 4. Notes

- Expected weighted on-base average (xwOBA) value for each batted ball is given by the xwoba. RData file trained by the author. Find more about the model here {LINK!!!!!!!}
- In the "Spray Charts" tab, one will find the filter "Part of the plate" with only two options, "Inside" and "Outside". This is subjective to the batter regardless of the batter handedness.
- If the user doesn't have access to any trackman baseball data but still want play with the app, the author provides the trackman data from CPBL (Taiwanese pro league) Opening Day 2023, "example_trackman_data.csv". It does not need to be processed by "Add_necessary_columns.R".
- The data in "Ump_data.csv" are collected from CPBL as well. However, these data are not open to public therefore the names of the umpires have been replaced by fake names.
  
