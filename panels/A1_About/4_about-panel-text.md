## New deployment system

Thank you for trying out our new, _in development_ deployment system for iNZight Lite. The new version has come around in response to many users experiencing issues connecting to Lite, and being unable to reconnect without performing a range of troubleshooting steps.

This new system uses a completely different AWS toolkit, so while we expect Lite to perform as well or better than previously, we cannot test every single feature, nor can we test a wide range of datasets. If you notice any issues, please report them to us, especially if you cannot replicate the issue [on the current main deployment](https://lite.docker.stat.auckland.ac.nz).

### Update plan

- ~~build new architecture~~
- ~~Test caching and stickiness is no longer causing connectivity issues after updates~~
- Test scaling performance - **if you have a class that uses iNZight, we would be happy to schedule a test of the new deployment's scaling capabilities**.
- Make necessary tweaks to autoscaling configuration
- Deploy a new production version of iNZight Lite. _This will have a new home / URL, so existing material will still point to the current/old version of Lite while we roll out the new system_.
- Once we are seeing good performance and uptake of the new system, we will issue a timeline for deprecating the old one. This will simply involve a redirect of the old URL to iNZight Lite's new home.

### Feedback and concerns

To help us configure the new deployment, please take a moment to complete the feedback form: https://forms.gle/RGC3eT2HKNsm8VL89. You're more than welcome to fill this out any time you notice a difference, as this will help us gauge if any changes are having the desired effect.

If you have any feedback (either about the new system or Lite in general), or have any concerns about how the changes might affect you, please reach out to us.

Contact: inzightlite_support@stat.auckland.ac.nz

## Getting Started

To get started, import a dataset using the **File** dropdown from the tabs above, and then choose either

- **Import Dataset** if you have your own data to explore, or

- **Dataset Examples** if you're just looking to explore the software using an example dataset.

Not sure what to do? We have made a [getting started video](https://www.stat.auckland.ac.nz/~wild/iNZightLite/IntroMovie.html) and there is help available within most modules. Just look for the green "help" button, usually located in the bottom left hand corner of the screen.

## About

**iNZight** is a simple data analysis system initially designed for high school students to help them explore data quickly and easily. It still serves this function, but other demands have seen extensive additions to its capabilities. For example, it now provides very flexible multivariate visualisation capabilities and supports maps, time series, and multivariable regression analysis.

**iNZight _Lite_** is an online version of the software accessible from any modern web browser, thus making it suitable to a wider range of users including non-Windows users who cannot install the desktop software. With Lite, you can import your own dataset or explore one of the many example datasets. You can then explore the data using visualizatin, transformation, and statistical analysis to find its hidden secrets.

If you want to provide feedback please fill in the [contact form](http://inzight.nz/support/contact?v=lite).

## The iNZight Project

The project is led by [Professor Chris Wild](http://www.stat.auckland.ac.nz/~wild) and has been primarily supported by the Department of Statistics at the University of Auckland, with additional support from Statistics New Zealand and the NZ Ministry of Education via Census at School.

If you are an expert programmer or statistician, you can contribute to the project by sending us feedback about our [R source code](https://github.com/iNZightVIT).
