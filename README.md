# NOTES

*yesod-generate assumes a canonical file structure based on the output of the yesod scaffolder.  if you move files around yesod-generate will fail.*
### TODOs

* test the output
* make it pretty
* add bootstrap option

# Yesod Generators

Generators to help kickstart yesod projects and promote best practices.

## Supported Generators

* model
* *nothing else at this time*

## Model Generation

    yesod-generate model <TableName> (<field Type [Maybe]>[, <field Type [Maybe]>])+

for example:

    yesod-generate model User name Text, age Int Maybe, bday Day Maybe, group GroupId

### Notes:

* put a comma after each field.
* can put Maybe after the type to denote a nullable type

### Supported Types

* Other persistent Ids
    * *note that any type ending in Id is assumed to be a persistent key and will be treated as such.*
* Bool 
* Double 
* Int 
* String  
    * *Note that we will assume you meant Text, since you should be.*
* Text 
* Day
* CountryCode 
    * *uses ISO3166 2 letter codes from iso3166-country-codes*
    * Gives an example of how to add in a simple, but totally alien type to persistent
* Image
    * **IF YOU ADD IMAGES, your JSON/VIEWs will be Ugly.  you are warned.**
    * naively stores image data in the database as a byte string

### possibly coming soon

* Int8 
* Int16 
* Int32 
* Int64 
* ByteString 
* Html 
* TimeOfDay 
* UTCTime 
