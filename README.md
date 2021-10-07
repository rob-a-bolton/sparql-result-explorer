# SPARQL Result Explorer
This is a simple program to load multiple SPARQL query result sets and compare using union/intersection/difference.  
Results are expected to be in JSON (as according to [SPARQL 1.1 Query Results JSON Format](https://www.w3.org/TR/sparql11-results-json/) specification).  


## Usage
Run the program like so to launch it:  
`clojure -M -m sparql-result-explorer.core`  
No command-line arguments or configuration file are needed (or supported).  

To perform union/intersection/difference, select multiple entries in the list of loaded files (ctrl+click to add/remove individual items to selection, shift click to select a range of files).  
The selected files must contain the same query variables.  