# bccr
Reads Data From Central Bank of Costa Rica

The Central Bank of Costa Rica provides many economic indicators on its website (http://www.bccr.fi.cr/indicadores_economicos_/), which can be 
downloaded in Microsoft Excel format one table at a time.

The objective of this package is to simplify the process of finding, downloading, and tidying the data. In particular, this package provides functions to:

* Search for indicators by partial name matching (say, "IPC" to find consumer price indices).
* Download several indicators in a single call, by providing their table numbers.
* Substitute commas and periods to match R's decimal representation.
* Tidy up the resulting table (one column per indicator).
* Create a date index for the tables.
* Change the frequency of time series (say, from daily to monthly).
* Merge all downloaded tables into a single data.table object.


## Disclosure:
This package is NOT endorsed by the Central Bank of Costa Rica. It is shared by the author to help fellow researchers in the task of obtaining
Costa Rican data; however, no warranty is provided as to the reliability of the package.
