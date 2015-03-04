Accounter - a simple book-keeping app
====================================

The application is written in Erlang and is using Yaws as webserver.

In the webui it is possible to do things like

- register new vouchers
- correct erroneous vouchers
- generate archivable reports
  - balance report
  - result report
  - journal
  - ledger
  - diff between result and budget
- generate one big combo report as an HTML file containing balance, result, journal, ledger and diff. This combo report is suitable to share to your board members, enabling them to get a good picture of the status by simply click around following the links between the different reports. This exported combo report does not require the app to be run.

Prerequisites
-------------

Get and install an Erlang system (http://www.erlang.org).
Get and install the Yaws web server (http://yaws.hyber.org).

Mac OS X
--------

>     port install erlang
>     port install yaws

Linux
-----

>     apt-get install erlang
>     apt-get install build-deps yaws

Windows
-------

>     http://www.erlang.org/download/otp_win32_17.4.exe
>     http://yaws.hyber.org/download/Yaws-1.99-windows-installer.exe


Installation
============

Clone the repository
--------------------

>     git clone https://github.com/hawk/accounter.git

Build
-----

>     cd accounter
>     autoconf
>     ./configure
>     make

Start the webserver
-------------------

>     bin/accounter --dir WorkDir


Open initial page in web browser
--------------------------------

>     firefox http://localhost:8080
