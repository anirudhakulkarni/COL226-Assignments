1. converDelimiters:
   1. take input and write custom output functions
      1. write - write given text at given location
      2. read - reads input from given file address and returns content. Without any formatting
   2. replacedelim - returns converted text when a string and delimeters are given
      1.
   3. convert delimiters
      1. call read function and store data
      2. call replacedelim and store its data in replacedtext
      3. write the replacedtext at location
2. csv2tsv:
   1. call convertdelimiters with custom inputs
3. tsv2csv:
   1. same as above
4. convertNewlines:
   1. read file and store the data
   2. call replaceNewlines function and store output
      1. replaceNewlines:
   3. write data to file
5. unix2dos:
   1. call above func with \r\n and \n
6. dos2unix:
   1. same as above
