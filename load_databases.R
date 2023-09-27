#Get Election Metadata
mydb <- connectDB(db_name="sda_elections")
rs <- dbSendQuery(mydb, "SELECT * FROM elections_metadata")
election_metadata <- fetch(rs,n=-1)
dbDisconnectAll()

#Get Party Metadata
mydb <- connectDB(db_name="sda_elections")
rs <- dbSendQuery(mydb, "SELECT id,shortname_de,shortname_fr,shortname_it,bfs_id,party_color,position_parliament FROM parties_metadata")
parties_metadata <- fetch(rs,n=-1)
dbDisconnectAll()

parties_metadata <- parties_metadata %>%
  filter(id !=10)

#Get areas metadata
mydb <- connectDB(db_name="sda_elections")
rs <- dbSendQuery(mydb, "SELECT * FROM areas_metadata")
areas_metadata <- fetch(rs,n=-1)
dbDisconnectAll()

#Get people metadata
mydb <- connectDB(db_name="sda_elections")
rs <- dbSendQuery(mydb, "SELECT id,firstname,lastname,gender,birthdate,place,picture FROM people_metadata")
people_metadata <- fetch(rs,n=-1)
dbDisconnectAll()

#Get profession
mydb <- connectDB(db_name="sda_elections")
rs <- dbSendQuery(mydb, "SELECT person_id,title,category FROM people_profession WHERE source = 'BFS 2023'")
people_profession <- fetch(rs,n=-1)
dbDisconnectAll()

people_profession <- people_profession %>%
  group_by(person_id) %>%
  summarise(title=paste(title,collapse=", "))

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

#Get Datawrapper Codes
mydb <- connectDB(db_name="sda_elections")
rs <- dbSendQuery(mydb, "SELECT * FROM datawrapper_codes")
datawrapper_codes <- fetch(rs,n=-1)
dbDisconnectAll()

