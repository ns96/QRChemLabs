# QRChemLabs

Quick Response Chemistry Labs -- A RShiny based web application allowing students to  input, check, and view general Chemistry Lab data. [QR Chem Labs](https://ns96.shinyapps.io/QRChem/)

## Introduction

One of the biggest challenges for students taking general chemistry labs is determining the validity of the data they collect. In the past, instructors often had to conduct the lab alongside students to demonstrate expected data and calculations. With QRChemLabs, instructors can quickly show students how their data should appear and, more importantly, allow them to input and check their data and calculations. It is primarily meant as a companion for in-person general chemistry labs. It can also serve as a data source for virtual general chemistry labs, as was the case during the COVID-19 pandemic. During the pandemic, students were able to easily generate data as they would have previously and submit reports as usual.

<img src="file:///C:/Users/Nathan/My%20Drive/General%20Chem/QRChem/QRChem01.png" title="" alt="" data-align="center">

## Installation

This code was primarily designed for deployment on [shinyapps.io](https://www.shinyapps.io/) and is not meant for local installation unless for development. Before deployment, input the correct database connection information into the config.bak.yml and rename it to config.yml. To setup the database, login into your MySQL/MariaDB instance and run the qrchem_bak.sql. Next add records to the "courses" table for your particular institution. 

## Technicals

This application is a good example of using Shiny dashboard and how to organize code using [Shiny - Modularizing Shiny app code](https://shiny.posit.co/r/articles/improve/modules/) and multiple files.
