# TODOs

* test the output
* make it pretty
* add bootstrap option

# Yesod Generators

Generators to help kickstart yesod projects and promote best practices.

## Supported Generators

* model
* *nothing else at this time*

## Model Generation

    yesod-generate model <TableName> (<field::{Type|Type?}>)+

for example:

    yesod-generate model User name::Text age::Int? bday::Day? group::GroupId

### Notes:

* append a '?' to a type annotation for nullable, aka Maybe, types.

### Supported Types

* Other persistent Ids
    * note that any type ending in Id is assumed to be a persistent key and will be treated as such.
    * boo
* Bool 
* Double 
* Int 
* Int8 
* Int16 
* Int32 
* Int64 
* Word8 
* Word16 
* Word32 
* Word64 
* String 
* Text 
* ByteString 
* Html 
* TimeOfDay 
* UTCTime 
* Day
