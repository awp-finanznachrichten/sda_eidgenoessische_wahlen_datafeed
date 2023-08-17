#Get Electiton Metadata
mydb <- connectDB(db_name="sda_elections")
rs <- dbSendQuery(mydb, "SELECT * FROM elections_metadata")
election_metadata <- fetch(rs,n=-1)
dbDisconnectAll()

#Get Party Metadata
mydb <- connectDB(db_name="sda_elections")
rs <- dbSendQuery(mydb, "SELECT id,shortname_de,shortname_fr,shortname_it,bfs_id,party_color FROM parties_metadata")
parties_metadata <- fetch(rs,n=-1)
dbDisconnectAll()

parties_metadata <- parties_metadata %>%
  filter(id !=10)

#Get areas metadata
mydb <- connectDB(db_name="sda_elections")
rs <- dbSendQuery(mydb, "SELECT * FROM areas_metadata")
areas_metadata <- fetch(rs,n=-1)
dbDisconnectAll()

#Get texts
mydb <- connectDB(db_name="sda_elections")
rs <- dbSendQuery(mydb, "SELECT * FROM status_text")
status_texts <- fetch(rs,n=-1)
dbDisconnectAll()

#Get Output Overview
mydb <- connectDB(db_name="sda_elections")
rs <- dbSendQuery(mydb, "SELECT * FROM output_overview")
output_overview <- fetch(rs,n=-1)
dbDisconnectAll()

output_overview <- output_overview %>%
  select(!last_update)

