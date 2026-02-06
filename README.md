# SHINE survey data cleaning and report generation tool

This tool is purpose-built for cleaning data from the Qualtrics-administered SHINE survey for primary and secondary schools in Scotland. It can be installed as an R package and is run as a Shiny app on a local or remote machine.

# Installation and running

## On a local machine

You can install from the `ScotlandSHINE` repository (from a user account with access) by using:

```
remotes::install_github("ScotlandSHINE/SHINEcleaning")
```

Running the app in an R session then requires:

```
> library(SHINEcleaning)
> run()
```

Alternatively, you can 'clone' the GitHub repository and open `SHINEcleaning.Rproj` in a new RStudio session. This allows you access to editing files and templates. With the `devtools` package installed the app can be run with:

```
devtools::load_all()
run()
```

## On a Shiny server

[Shiny server](https://posit.co/download/shiny-server/) is a way of continually running (multiple) shiny apps on a machine, allowing users to access a remote shiny app using a web browser.

[Here is a handy guide](https://www.charlesbordet.com/en/guide-shiny-aws/) to deploying a shiny app via a shiny server (in this case using an AWS compute instance). This can work on any remotely-accessible Linux machine.

To run the installed package as an app, first create a directory for your app (e.g. `/home/[user]/shinemh`) and point to it in `/etc/shiny-server/shiny-server.conf`:

```server {
  location / {

    run_as [user];
    site_dir /home/[user]/shinemh;
    log_dir /home/[user]/shinemh/log;

    directory_index off;
  }
}
```

Within this new directory, create an `app.R` file which runs your app:

```
SHINEcleaning::run()
```

At the shiny server location (default port 3838) this should now be running the dashboard! See the above guide for walk-throughs on how to instlal/customise this.
