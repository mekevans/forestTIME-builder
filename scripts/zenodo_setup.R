# This is a script for programmatic access to a Zenodo record.
# I am not 100% sure how this step will fit into the wider workflow -
# it depends on whether we end up using gh actions and/or Zenodo,
# and, if so, whether that's one record or 51 records. 

library(zen4R)

zenodo <- ZenodoManager$new(
  token = Sys.getenv("ZENODO_TOKEN"), 
  logger = "INFO" # use "DEBUG" to see detailed API operation logs, use NULL if you don't want logs at all
)


# I'm not sure how to programmatically set access to restricted
# What you can do is create the draft and then use the web interface to change the access setting
# and then publish it.

myrec <- ZenodoRecord$new()
myrec$setTitle("forestTime_Tables_Dev")
myrec$setResourceType("software")
myrec$setPublicationDate(Sys.Date())
myrec$addCreator(firstname = "Renata", lastname = "Diaz")
myrec$setPublisher("Zenodo")
myrec <- zenodo$depositRecord(myrec, publish = FALSE)
zenodo$uploadFile("README.md", record = myrec)
# Go online and change the access before running this:
zenodo$publishRecord(myrec$id)
