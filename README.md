# QRChemLabs: Enhancing Data Validation and Learning in General Chemistry Labs

Quick Response Chemistry Labs -- A RShiny based web application allowing students to  input, check, and view general Chemistry Lab data. [QR Chem Labs](https://ns96.shinyapps.io/QRChem/)

## Introduction

QRChemLabs emerges as a valuable tool designed to tackle a significant hurdle for students in general chemistry labs: ensuring the validity of their collected data and the accuracy of their calculations. Traditionally, instructors would demonstrate expected outcomes by performing the lab alongside students. QRChemLabs streamlines this process, offering a dynamic platform for both instructors and students.

The core functionality of QRChemLabs lies in its ability to quickly present students with how valid data and calculations should appear. This provides students with a clear benchmark against which they can compare their own results. More importantly, the platform allows students to directly input their experimental data and check their subsequent calculations. This interactive feedback loop is crucial for students to identify errors, understand the impact of those errors, and build confidence in their laboratory work.

Primarily intended as a supplement for in-person general chemistry labs, QRChemLabs seamlessly integrates into the traditional lab environment. However, its utility extended significantly during the COVID-19 pandemic, where it proved to be an effective data source for virtual general chemistry labs. This allowed students to continue engaging with the data analysis and calculation aspects of experiments even when in-person attendance wasn't possible. During this period, students could easily generate randomized data sets through the platform, perform the necessary calculations, and complete lab reports as they would with physical data.

<img src="QRChem01.png" title="" alt="" data-align="center">

The most recent iteration of QRChemLabs introduces a sophisticated feature for Chemistry II labs: the generation of scientific abstracts using large language models (LLMs) from Google, ChatGPT, and DeepSeek. This addresses another common challenge for students – writing a concise and accurate scientific abstract. By showcasing examples of well-structured and valid abstracts, instructors can provide students with a clear model, helping them to develop this essential scientific writing skill. This feature leverages the capabilities of cutting-edge AI to further enhance the learning experience within the chemistry curriculum.

<img src="QRChem02.png" title="" alt="" data-align="center">

<img src="QRChem03.png" title="" alt="" data-align="center">

## Installation

This code was primarily designed for deployment on [shinyapps.io](https://www.shinyapps.io/) and is not meant for local installation unless for development. Before deployment, rename config.bak.yml to config.yml then input the correct database connection information. To setup the database, login into your MySQL/MariaDB instance and run the qrchem_bak.sql. Next, add records to the "Courses", "Semesters", and "UserPins"  tables for your particular institution. You should delete any existing records already in database tables. I recommend the [HeidiSQL](https://www.heidisql.com/) program for managing the MySQL database. 

## Technicals

This application is a good example of using Shiny dashboard and how to organize code using modules ([Shiny - Modularizing Shiny app code](https://shiny.posit.co/r/articles/improve/modules/)) and multiple files. Being still in development, there are serveral features that still need to be implemented.

1. Add "Setup"" module to manage settings such as current semester, courses, valid pin etc ...

2. Review all modules and correctly display significant figures.
