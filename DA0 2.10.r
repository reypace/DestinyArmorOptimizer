#2.3 added class selection and labels for item location. 2.4 adds load bars and updates manifest
#2.5 (dated 1-6-22) fixes the errors cause when bungie switched to crossplay IDs
#2.6 adds the linked version of oauth for distribution online
#2.9 adds support for non xbox people
#2.10 sorts exotic items and also removes <current light level items from the list
library(shiny)
library(pracma) #provides error function
library(ggplot2)
library(reshape2)
library(gridExtra)
library(directlabels)
library(RSQLite)
library(httr)
library(jsonlite)
library(shinyjs)
library(stringr)

convHash=function(hash)
{
  if(hash>2147483647)
  {
    hash=hash- 4294967296
  }
  return(hash)
}

indicator=function(x)
{
  if(x==TRUE)
  {
    result=1
  }
  else{
    result=0
  }
  return(result)
}


    ui = fluidPage(
       useShinyjs(),
       titlePanel("DAO-Destiny Armor Optimizer"),
       "By J. Pace",
      # htmlOutput(''),#I don't know why, but this gives me the linebreak i want
      # 'NOTICE:',
      #  htmlOutput(''),
      # 'DESTINY/BUNGIE is down today for maintenence until ~12PM PST. This app (like all other services that require the bungie server) will also be down until that time',
      htmlOutput(''),
       #uiOutput("tab"),
       shiny::actionButton(inputId='auth', label="Authenticate", 
                          icon = icon("th")), 
                        #  onclick ="window.location.href = 'https://www.bungie.net/en/OAuth/Authorize?client_id=33959&response_type=code'"),
#window.open('http://google.com', '_blank')")

    
      shinyjs::hidden(
        textInput('name',"Enter your Bungie Screen Name"),
        textInput('nameCode','Enter your Bungie 4 digit code'),
        selectInput('classes', 'Select Class to Optimize', c('Titan','Hunter','Warlock'), selectize=TRUE,selected=TRUE),
        actionButton('go1','Grab All Armor from Server',icon("arrow-right"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        uiOutput('columns',width=500)
        ),
        splitLayout(cellWidths = c("50%", "50%"), 
          shinyjs::hidden(actionButton('go2','Run all Armor Combinations',icon("arrow-right"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
         shinyjs::hidden( checkboxInput("includeOldArmor", "Include Sunsetted Armor?", FALSE))
          ),
        shinyjs::hidden(
        textOutput('textOut2'),
        htmlOutput(''),
        selectInput('mobility', 'Select Desired Mobility', c( c(0,1:10*10)), selectize=TRUE,selected=TRUE),
        selectInput('discipline', 'Select Desired Discipline', c( c(0,1:10*10)), selectize=TRUE,selected=TRUE),
        selectInput('intellect', 'Select Desired Intellect', c( c(0,1:10*10)), selectize=TRUE),
        selectInput('strength', 'Select Desired Strength', c( c(0,1:10*10)), selectize=TRUE),
        selectInput('recovery', 'Select Desired Recovery', c(c(0,1:10*10)), selectize=TRUE),
        selectInput('resiliance', 'Select Desired Resiliance', c( c(0,1:10*10)), selectize=TRUE),
        selectInput('masterwork','How many armor pieces are you willing to masterwork?',c(0:5),selectize=TRUE,selected=TRUE),
        selectInput('traction','Will you be running traction?',c('no','yes'),selectize=TRUE,selected=TRUE),
        selectInput('radiant','Will you be running radiant light?',c('no','yes'),selectize=TRUE,selected=TRUE),
        selectInput('friends','Will you be running powerful friends?',c('no','yes'),selectize=TRUE,selected=TRUE),
        actionButton('go3','Seek armor sets that fit my criteria',icon("arrow-right"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        ),#end hidden
                
    #enter name and authenticate
        
       
     
    #select exotic
                 
                        
      #                   actionButton('go2','Run all Armor Combinations',icon("arrow-right"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    #select stats
           
            #  mainPanel("Welcome to the Destiny II Armor Optimizer. Please click the 'Authenticate' button below, and follow the instructions to login and approve this app. Afterwords, you'll see the inputs appear. No warranty nor promise of support - use at own risk. App created, owned, etc. by J. Pace, 2020-ever"),
           textOutput('textOut'),
uiOutput('columnsGear',width=500),
  hidden(
   actionButton('go4','Press to pull info on selected armor set',icon("arrow-right"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),#end hidden
 tableOutput("value3")
   )#end UI
 server = function(input, output, session) {
                            output$textOut=renderText({ paste0(
                                                           "Welcome to the Destiny II Armor Optimizer. Please click the 'Authenticate' button above, and follow the instructions to login and approve this app. Afterwords, you'll see the inputs appear. No warranty nor promise of support - use at own risk App created, owned, etc. by J. Pace, 2020-ever"    
                                                              
                                                               )})  #makes the text output
                        output$textOut2=renderText({paste0('(Make sure to re-run combos if you select a different exotic)')})
  #now we try to get the url. if we get it and it has 'code in it' display the thingy
   runjs('var testObject =  window.location.href;Shiny.onInputChange("today_var",testObject);')
     urlData=isolate(input$today_var)
     
   observeEvent(input$today_var,{
        print(isolate(input$today_var))
        if(grepl('127',isolate(input$today_var)))
        {
        #if the code was found, show the UI elements
        shinyjs::hide('auth')
        shinyjs::hide('textOut')
        shinyjs::show('name')
        shinyjs::show('nameCode')
        shinyjs::show('classes')
        shinyjs::show('go1')
          observeEvent(input$go1,{
            
        shinyjs::hide('name')
        shinyjs::hide('nameCode')
        shinyjs::hide('classes')
        shinyjs::hide('go1')
          shinyjs::show('includeOldArmor')
          shinyjs::show('go4')
          shinyjs::show('columns')
          shinyjs::show('textOut2')
          shinyjs::show('go2')
          shinyjs::show('mobility')
          shinyjs::show('discipline')
          shinyjs::show('intellect')
          shinyjs::show('strength')
          shinyjs::show('recovery')
          shinyjs::show('resiliance')
          shinyjs::show('masterwork')
         # shinyjs::show('traction')
          shinyjs::show('radiant')
          shinyjs::show('friends')
          shinyjs::show('go3')
          })#end observe go1
          
        
        print('yes')
        }
        })

   
   
   observeEvent(input$auth, {
     if (input$auth == 0)   #this ingenious little bit of code stops the app from running prior to the first click of button
              return()
     
       runjs("window.location.href = 'https://www.bungie.net/en/OAuth/Authorize?client_id=33959&response_type=code'")

   })


  
        observeEvent(input$go1, {
          
          if (input$go1 == 0)   #this ingenious little bit of code stops the app from running prior to the first click of button
              return()
             
    withProgress(message = 'Preparing to pull data...', min=0,max=100, value = 0, { 
          
          
          
apiKey <- "BLAM (censored)"

#oh, type your user name
userName=isolate(input$name)
userCode=as.integer(isolate(input$nameCode))

#try this method... This works! Gets me the xbox ID which is all I really need anyway
res2=POST("https://www.bungie.net/Platform/Destiny2/SearchDestinyPlayerByBungieName/All/",
          add_headers('X-API-Key'= apiKey), encode='json', accept='text/xml',
          body=list(displayName=userName,displayNameCode=userCode))
      out=fromJSON(rawToChar(res2$content))
      
      #choose any of their memberships
      memberShipNumber=out$Response$membershipType[1]
      #selectOut=which(out$Response$membershipType==1)
      xboxID=out$Response$membershipId[1]
      
      
      
      print(xboxID)
      print(169)
#Step 1: Authenticate

#authorize and get client code
#res4=GET("https://fiakxx-jesse-pace.shinyapps.io/deploytest/?code=cec696fe6a13b7339a6c29f827c18a74",
 #        add_headers('X-API-Key'= apiKey))
#rawToChar((res4$content))

url='https://fiakxx-jesse-pace.shinyapps.io/deploy/?code=f56c02982bc4f0e1d867ff114a572ea8'#isolate(input$today_var) 
code=unlist(strsplit(url,'='))[2]
print(url)
#04fed97af60000ec8e54a2a200000001

#b4a8add96b1939ec3da56edca9f8ad8c


test=POST("https://www.bungie.net/Platform/App/OAuth/token/",
          add_headers('Content-Type'= 'application/x-www-form-urlencoded','X-API-Key'= apiKey),
          body=paste0('client_id=33959&grant_type=authorization_code&code=',code))
          #body='client_id=33959&grant_type=authorization_code&code=b4a8add96b1939ec3da56edca9f8ad8c')
keyRaw=rawToChar(test$content)
print(189)

  #clean up the key
      testing=noquote(keyRaw)
      testing2=unlist(strsplit(testing,':'))[2]
      testing3=noquote(testing2)
      testing4=strsplit(testing3,',')
      testing5=as.character(noquote(unlist(testing4)))
      key=gsub('\"','',testing5)[1]
      
print(key)
#Get and loop over all items on all (up to 3) characters)
      
      #pulls character info
      res4=GET(paste0("https://www.bungie.net/platform/Destiny2/",memberShipNumber,"/Profile/",xboxID,"/?lc=en&components=100"), #the ?1c=en&components is code that lets me do components. 100 is default (look on site for profiel)
               add_headers('X-API-Key'= apiKey))
      temp=fromJSON(rawToChar((res4$content)))
      charID=temp$Response$profile$data$characterIds
      print(paste0("https://www.bungie.net/platform/Destiny2/",memberShipNumber,"/Profile/",xboxID,"/?lc=en&components=100"))
   
      equipInstance=NULL
      inventoryInstance=NULL
      equipHash=NULL
      inventoryHash=NULL
      for(i in 1:length(charID))# loop over all chars
      {
      
      #get equiped and inventory itesm
      res4=GET(paste0("https://www.bungie.net/platform/Destiny2/",memberShipNumber,"/Profile/",xboxID,"/Character/",charID[i],"/?components=201,205"),
               add_headers('X-API-Key'= apiKey,Authorization = paste('Bearer', key, sep = " ")))
      out=fromJSON(rawToChar(res4$content))
    
      equipInstance=c(equipInstance, out$Response$equipment$data$items$itemInstanceId)
      equipHash=c(equipHash,out$Response$equipment$data$items$itemHash)
      
      inventoryInstance=c(inventoryInstance,out$Response$inventory$data$items$itemInstanceId)
      inventoryHash=c(inventoryHash,out$Response$inventory$data$items$itemHash)
      }





#get vault
res5=GET(paste0("https://www.bungie.net/platform/Destiny2/",memberShipNumber,"/Profile/",xboxID,"/?components=102"),
         add_headers('X-API-Key'= apiKey,Authorization = paste('Bearer', key, sep = " ")))


out3= fromJSON(rawToChar( res5$content))

vaultInstance=out3$Response$profileInventory$data$items$itemInstanceId
vaultHash=out3$Response$profileInventory$data$items$itemHash
#View(out3$Response$profileInventory$data$items)

#make id for vault, equiped, or inventory
equID=rep('equip',length(equipHash))
vauID=rep('vault',length(vaultHash))
invID=rep('inventory',length(inventoryHash))



allItems=cbind.data.frame(c(equipHash,inventoryHash,vaultHash),c(equipInstance,inventoryInstance,vaultInstance),c(equID,invID,vauID))
colnames(allItems)<-c('hash','instance','location')
# need to pasty the id's so I can pull
#try to pull the information from the server
#item_components = '302,304,307'

storeAll=as.data.frame(matrix(ncol=24,nrow=nrow(allItems)))

colnames(storeAll)<-c('class','itemSlot','itemName','location','rarity','mobility','resiliance','recovery','discipline','intellect','strength',
                      "mod1" , "mod2",  "mod3"  ,"mod4" , "mod5",  "mod6" , "mod7"  ,"mod8" , "mod9"  ,"mod10",'mod11','energy'
                      )

#grab manifest
storeAll$instance=allItems$instance


result <- GET("https://www.bungie.net/Platform/Destiny2/Manifest/ ", 
                add_headers(Authorization = paste("X-API-Key", apiKey))
              )

  out2= fromJSON(rawToChar( result$content))
  address=out2$Response$mobileWorldContentPaths$en
  
  addressFull=(paste0("https://www.bungie.net",(address)))
  download.file(addressFull,'manifest.zip',mode='wb',quiet=TRUE)
  database=unzip('manifest.zip')
           

con=dbConnect(RSQLite::SQLite(),database)
  a= dbReadTable(con,'DestinyInventoryItemDefinition')   #using this package, we have to read the table first
  aa= dbReadTable(con,'DestinyPowerCapDefinition')
 
####
    #note I could optimize the below code by subsetting on items that are are armor(so I'd basically split the loop in two
      #1: loop over and pull item info from manifest
      #2: subset on armor
      #3: loop over armor and pull from API

for(i in 1:nrow(allItems))
{
  itemInstance=allItems$instance[i]
  itemHash=allItems$hash[i]#304
 # browser()
  #storeAll$instance[i]=allItems$instance[i]
  #browser()
  
    b=fromJSON(a[ which(a$id== convHash(itemHash)),2]) #grab item hash info

  storeAll$rarity[i]=unlist(strsplit(b$itemTypeAndTierDisplayName,' '))[1]
  storeAll$class[i]=b$classType #0 is titan, 1 is hunter, 2 is wizzy
  storeAll$itemSlot[i]=b$itemTypeDisplayName
  storeAll$itemName[i]=b$displayProperties$name
  storeAll$location[i]=allItems$location[i]
  
  #storeAll$powerCap[i]=b$quality$versions$powerCapHash #this gives me the hash. then i need to conver it?
   if(length(b$quality$versions$powerCapHash[1])>0){
   storeAll$powerCap[i]=fromJSON(   aa[which(aa$id==convHash((b$quality$versions$powerCapHash[1]))),2])$powerCap}
    

  incProgress(1/nrow(allItems))
}
  
    })#end withprogress
       
 #   write.csv(storeAll,'storeallz.csv')
  
  classy=input$classes
  classy[classy=='Titan']<-0
  classy[classy=='Hunter']<-1
  classy[classy=='Warlock']<-2
  classy2=as.integer(as.character(classy))


armorOnly=storeAll[storeAll$itemSlot%in%c('Chest Armor','Helmet','Leg Armor','Hunter Cloak','Titan Mark','Warlock Bond','Gauntlets')&storeAll$class==classy2&storeAll$powerCap> (1340*!isolate(input$includeOldArmor)),]

withProgress(message = 'Pulling data details...', min=0,max=nrow(armorOnly), value = 0, { 
     
for(i in 1:nrow(armorOnly))
    {
  itemInstance=armorOnly$instance[i]
 #
  itemHash=armorOnly$hash[i]#304

res4=GET(paste0('https://www.bungie.net/platform/Destiny2/1/Profile/',xboxID,'/Item/',itemInstance,'/?components=300,305,304'),
           add_headers('X-API-Key'= apiKey,Authorization = paste('Bearer', key, sep = " ")))
  
  out= fromJSON(rawToChar( res4$content))
  
  
  socketInfo=out$Response$sockets$data$sockets$plugHash
  socketInfo[is.na(socketInfo)]<-481675395 #this is the code for 'no mod socketed' which serves our needs fine
    
    armorOnly$energy[i]=   max(out$Response$instance$data$energy$energyCapacity,0)
    #Store socket info
      ##grab socket hash info(s)
    if(length(socketInfo)>0){armorOnly$mod1[i]=fromJSON(a[ which(a$id== convHash(socketInfo[1])),2])$displayProperties$name}
    if(length(socketInfo)>1){armorOnly$mod2[i]=fromJSON(a[ which(a$id== convHash(socketInfo[2])),2])$displayProperties$name}
    if(length(socketInfo)>2){armorOnly$mod3[i]=fromJSON(a[ which(a$id== convHash(socketInfo[3])),2])$displayProperties$name}
    if(length(socketInfo)>3){armorOnly$mod4[i]=fromJSON(a[ which(a$id== convHash(socketInfo[4])),2])$displayProperties$name}
    if(length(socketInfo)>4){armorOnly$mod5[i]=fromJSON(a[ which(a$id== convHash(socketInfo[5])),2])$displayProperties$name}
    if(length(socketInfo)>5){armorOnly$mod6[i]=fromJSON(a[ which(a$id== convHash(socketInfo[6])),2])$displayProperties$name}
    if(length(socketInfo)>6){armorOnly$mod7[i]=fromJSON(a[ which(a$id== convHash(socketInfo[7])),2])$displayProperties$name}
    if(length(socketInfo)>7){armorOnly$mod8[i]=fromJSON(a[ which(a$id== convHash(socketInfo[8])),2])$displayProperties$name}
    if(length(socketInfo)>8){armorOnly$mod9[i]=fromJSON(a[ which(a$id== convHash(socketInfo[9])),2])$displayProperties$name}
    if(length(socketInfo)>9){armorOnly$mod10[i]=fromJSON(a[ which(a$id== convHash(socketInfo[10])),2])$displayProperties$name}
    if(length(socketInfo)>10){armorOnly$mod11[i]=fromJSON(a[ which(a$id== convHash(socketInfo[11])),2])$displayProperties$name}
    #okay, a little jankey, but I will make 10 mod spots, then see if any are 'str, int, mob', etc. And reduce stats by that number
      #god I guess I also need to look for powerful friends and the strength one
  if(length(b$quality$versions$powerCapHash)>0){
   armorOnly$powerCap[i]=fromJSON(   aa[which(aa$id==convHash((b$quality$versions$powerCapHash))),2])$powerCap}

          armorOnly$mobility[i]=out$Response$stats$data$stats$`2996146975`$value
    armorOnly$resiliance[i]=out$Response$stats$data$stats$`392767087`$value
    armorOnly$recovery[i]=out$Response$stats$data$stats$`1943323491`$value
    armorOnly$discipline[i]=out$Response$stats$data$stats$`1735777505`$value
    armorOnly$intellect[i]=out$Response$stats$data$stats$`144602215`$value
    armorOnly$strength[i]=out$Response$stats$data$stats$`4244567218`$value

    
  incProgress(1)
 
}
 
})#end with progress



#subset to only include weapons
  armorDatabase=armorOnly[armorOnly$itemSlot%in%c('Chest Armor','Helmet','Leg Armor','Hunter Cloak','Titan Mark','Warlock Bond','Gauntlets'),]
  #set NA in modlist to ''
    armorDatabase$mod1[is.na(armorDatabase$mod1)]<-''
    armorDatabase$mod2[is.na(armorDatabase$mod2)]<-''
    armorDatabase$mod3[is.na(armorDatabase$mod3)]<-''
    armorDatabase$mod4[is.na(armorDatabase$mod4)]<-''
    armorDatabase$mod5[is.na(armorDatabase$mod5)]<-''
    armorDatabase$mod6[is.na(armorDatabase$mod6)]<-''
    armorDatabase$mod7[is.na(armorDatabase$mod7)]<-''
    armorDatabase$mod8[is.na(armorDatabase$mod8)]<-''
    armorDatabase$mod9[is.na(armorDatabase$mod9)]<-''
    armorDatabase$mod10[is.na(armorDatabase$mod10)]<-''
    armorDatabase$mod11[is.na(armorDatabase$mod11)]<-''
  
  #adjust all armor by the mods socketed into it
 # C('Radiant Light','Powerful Friends','Traction','Recovery Mod','Mobility Mod','Resilience Mod','Intellect Mod','Strength Mod','Discipline Mod')
armorDatabase2=armorDatabase #for security/safety
  #loop over armor database and subtract for the mods they have
  for(i in 1:nrow(armorDatabase2))
  {
    modList= c("mod1" , "mod2",  "mod3"  ,"mod4" , "mod5",  "mod6" , "mod7"  ,"mod8" , "mod9"  ,"mod10",'mod11')
    armorDatabase2$mobility[i]=armorDatabase2$mobility[i]-0*indicator(any(armorDatabase2[i,modList]=='Traction'))-20*indicator(any(armorDatabase2[i,modList]=='Powerful Friends'))-10*indicator(any(armorDatabase2[i,modList]=='Mobility Mod'))-5*indicator(any(armorDatabase2[i,modList]=='Minor Mobility Mod'))-max(armorDatabase2$energy[i]-9,0)*2
    
    armorDatabase2$recovery[i]=armorDatabase2$recovery[i]-10*indicator(any(armorDatabase2[i,modList]=='Recovery Mod'))-5*indicator(any(armorDatabase2[i,modList]=='Minor Recovery Mod'))-max(armorDatabase2$energy[i]-9,0)*2
    armorDatabase2$resiliance[i]=armorDatabase2$resiliance[i]-10*indicator(any(armorDatabase2[i,modList]=='Resiliance Mod'))-5*indicator(any(armorDatabase2[i,modList]=='Minor Resiliance Mod'))-max(armorDatabase2$energy[i]-9,0)*2
    armorDatabase2$intellect[i]=armorDatabase2$intellect[i]-10*indicator(any(armorDatabase2[i,modList]=='Intellect Mod'))-5*indicator(any(armorDatabase2[i,modList]=='Minor Intellect Mod'))-max(armorDatabase2$energy[i]-9,0)*2
    armorDatabase2$discipline[i]=armorDatabase2$discipline[i]-10*indicator(any(armorDatabase2[i,modList]=='Discipline Mod'))-5*indicator(any(armorDatabase2[i,modList]=='Minor Discipline Mod'))-max(armorDatabase2$energy[i]-9,0)*2
    armorDatabase2$strength[i]=armorDatabase2$strength[i]-10*indicator(any(armorDatabase2[i,modList]=='Strength Mod'))-5*indicator(any(armorDatabase2[i,modList]=='Minor Strength Mod'))-20*indicator(any(armorDatabase2[i,modList]=='Radiant Light'))-max(armorDatabase2$energy[i]-9,0)*2

    }
          
          
#browser()
          
                          
          
          
          
                                 
                                  exoticList=armorDatabase2[ armorDatabase2$rarity=='Exotic',]
                                
                                  #sort by type, then by name
                                  helmets=exoticList[exoticList$itemSlot=='Helmet',]
                                  gauntlets=exoticList[exoticList$itemSlot=='Gauntlets',]
                                  chest=exoticList[exoticList$itemSlot=='Chest Armor',]
                                  legs=exoticList[exoticList$itemSlot=='Leg Armor',]
                                  
                                  helmets=helmets[order(helmets$itemName),]
                                  gauntlets=gauntlets[order(gauntlets$itemName),]
                                  chest=chest[order(chest$itemName),]
                                  legs=legs[order(legs$itemName),]
                                  
                                  exoticList=rbind.data.frame(helmets,gauntlets,chest,legs)
                                  
                                    
                                  output$columns = renderUI({  #get exotic selection
                                      #mydata = get(input$dataset)
                                      selectInput('columns2', 'Select your Exotic', width=700,paste0(exoticList$itemName,",",exoticList$itemSlot,",mob=",exoticList$mobility,",res=",exoticList$resiliance,',rec=',exoticList$recovery,',dis=',exoticList$discipline,',int=',exoticList$intellect,',str=',exoticList$strength) )
                                    })
                           
                           
         
                         
                         
                      observeEvent(input$go2,{
                         if (input$go2 == 0)   #this ingenious little bit of code stops the app from running prior to the first click of button
                          return()
                            
       
                   observeEvent(input$columns2, {
                    exoticSelected=isolate(input$columns2)
                       })  
                              
                             
                                  exoticSelected=isolate(input$columns2)
                                  exoticParse=strsplit(exoticSelected,',')
                                  exoticParse2=unlist(strsplit(as.character(unlist(exoticParse)),'='))
                            
                                  exoticSelection=which(exoticList$itemName==exoticParse2[1]&exoticList$mobility==as.numeric(as.character(exoticParse2[4]))&
                                    exoticList$resiliance==as.numeric(as.character(exoticParse2[6]))&
                                    exoticList$recovery==as.numeric(as.character(exoticParse2[8]))&
                                    exoticList$discipline==as.numeric(as.character(exoticParse2[10]))&
                                    exoticList$intellect==as.numeric(as.character(exoticParse2[12]))&
                                    exoticList$strength==as.numeric(as.character(exoticParse2[14]))
                                     )
                     
                              #Exotic choice (integer)
                                exoticChoice=exoticSelection

                                
                                
                                
                            #Compute stuff based on their choices
                                  exoticInfo=exoticList[exoticChoice,]
   #________________________                          
                              
          ###################################################################
                          # Begin math
                        ####
                          
                          #armor to search is head,chest,arm,legs - whichever exotic is
                          armorSet=c('Helmet','Gauntlets','Chest Armor','Leg Armor')
                          armorToSearch=armorSet[!(armorSet%in%exoticInfo$itemSlot)]
                          
                          
                          #simple algorithm, find max recov, then from that, find max mobil, then max resil
                          
                          #find the number of combinations and then execute the search across them
                          #I want to know which of the xxxhave the minimum recovery. THen i'll look at those for my mobility
                       
                          
                          numOne=nrow(armorDatabase2[ armorDatabase2$itemSlot== armorToSearch[1],])
                          numTwo=nrow(armorDatabase2[ armorDatabase2$itemSlot== armorToSearch[2],])
                          numThree=nrow(armorDatabase2[ armorDatabase2$itemSlot== armorToSearch[3],])
                          
                          
                          
                          storage=matrix(nrow=numOne*numTwo*numThree,ncol=9)
                          mode(storage)<-'integer'
                          colnames(storage)=c('recovery','mobility', 'intellect', 'resiliance','discipline','strength',armorToSearch[1],armorToSearch[2],armorToSearch[3])
                          
                          one=armorDatabase2[ armorDatabase2$itemSlot== armorToSearch[1]&armorDatabase2$rarity!='Exotic',]
                          two=armorDatabase2[ armorDatabase2$itemSlot== armorToSearch[2]&armorDatabase2$rarity!='Exotic',]
                          three=armorDatabase2[ armorDatabase2$itemSlot== armorToSearch[3]&armorDatabase2$rarity!='Exotic',]
                          
                          numOne=nrow(one);numTwo=nrow(two);numThree=nrow(three)
                        
                          #loop over gloves, then chest, then legs
                          i=1
                          withProgress(message = 'Running all possible armor combos...', min=0,max=nrow(one), value = 0, { 
                          
                          for(k in 1:nrow(one) )
                          {
                            for(l in 1:nrow(two))
                            {
                              for(p in 1:nrow(three))
                              {
                                storage[i,1]=one[k,'recovery']+two[l,'recovery']+three[p,'recovery']
                                storage[i,2]=one[k,'mobility']+two[l,'mobility']+three[p,'mobility']
                                
                                storage[i,3]=one[k,'intellect']+two[l,'intellect']+three[p,'intellect']
                                
                                storage[i,4]=one[k,'resiliance']+two[l,'resiliance']+three[p,'resiliance']
                                storage[i,5]=one[k,'discipline']+two[l,'discipline']+three[p,'discipline']
                                storage[i,6]=one[k,'strength']+two[l,'strength']+three[p,'strength']
                                storage[i,7]=k
                                storage[i,8]=l
                                storage[i,9]=p
                                i=i+1
                              }
                            }
                            print(k)
                            incProgress(1)
                          }  
                          
                          })#end with progress
                                  
                                 
                                  
        
                      observeEvent(input$go3,{
                         if (input$go3 == 0)   #this ingenious little bit of code stops the app from running prior to the first click of button
                          return()
                            #Get user inputs and then go on to the math
                                                  #        browser()
                                NumMasterworks=as.numeric(as.character(input$masterwork))   #choose integer (each gives +2 to all)
                                 # browser()
                                Traction=ifelse(input$traction=='no',0,1) #yes or no (1 or 0)
                                  
                                RadiantLight=ifelse(input$radiant=='no',0,1) #yes or no
                                  
                                PowerfulFriends=ifelse(input$friends=='no',0,1) #yes or no
                                  
                                
                                wantedInt=as.numeric(as.character(input$intellect))
                                #browser()
                                
                                wantedRes=as.numeric(as.character(input$resiliance))
                                
                                wantedMob=as.numeric(as.character(input$mobility))
                                  
                                wantedRec=as.numeric(as.character(input$recovery))
                                  
                                wantedDis=as.numeric(as.character(input$discipline))
                                
                                wantedStr=as.numeric(as.character(input$strength))
                              #  browser()
                                
                                      
                         
                                                
                      
                          
                          
                          # Do math
                          #needed numbers: 
                          A=max(wantedInt- exoticInfo$intellect-2*NumMasterworks,0) #never go below 0
                          B=max(wantedRes- exoticInfo$resiliance-2*NumMasterworks,0)
                          C=max(wantedMob- exoticInfo$mobility-2*NumMasterworks-20*PowerfulFriends-15*Traction,0)
                          D=max(wantedRec- exoticInfo$recovery-2*NumMasterworks,0)
                          E=max(wantedDis- exoticInfo$discipline-2*NumMasterworks,0)
                          F=max(wantedStr- exoticInfo$strength-2*NumMasterworks-20*RadiantLight,0)
                          
                          #The sum of the stats that I want -50 is a hit
                          #I.e., if I want a, b, c, and whatever for c and d and e
                          #then I need a+b+c-50 to equal ideal A+B+C
                          #furthermore, I need to use that 50 wisely
                          #let A-a= aa. and B-b=bb and C-c= cc
                          
                          #I need aa+bb+cc <= 50
                          #so, for calculating aa, bb, and cc, I CANNOT use absolute values. if A-a>0 then set a =0
                          #i.e., aa= max(0,A-a)
                          #that means that if a is bigger than A, A-a would return negative, but max will give me 0
                          
                          #find subset of armor that meets the criteria (if there are any)
                          aa=pmax(0,A- storage[,'intellect'])
                          bb=pmax(0,B - storage[,'resiliance'])
                          cc=pmax(0,C- storage[,'mobility'])
                          dd=pmax(0,D- storage[,'recovery'])
                          ee=pmax(0,E- storage[,'discipline'])
                          ff=pmax(0,F- storage[,'strength'])
                          
                          
                          which(aa+bb+cc+dd+ee+ff<=50)
                          
                          
                          #what we actually need, though, are some whole numbers.... but I need to solve where. It would be at the original step?
                          #9+11=20, but I'd need 2 mods to get to 20, whereas 10 and 10 would be better
                          
                          
                          
                          #well, the <=50 is a first pass. Then from that list I can do more intensive shit
                          #     subList=cbind.data.frame(storage,aa,bb,cc,dd,ee,ff)[which(aa+bb+cc+dd+ee+ff<=50),]
                          #  which(subList[,
                          
                          
                          #okay, so basically I can look at each point above 0 as a wasted point on my aa,bb...
                          #i.e., if dd is 15, then it takes 20 points (2 perks) to correct. So round it all up to nearest upper 10
                          #then sum, see if that's 50
                          #54+10-54%%10
                          #so, I need aa+(10-54Mod(1))
                          aa2=ifelse(aa==0,0,aa+(10-aa%%10) )
                          bb2=ifelse(bb==0,0,bb+(10-bb%%10) )
                          cc2=ifelse(cc==0,0,cc+(10-cc%%10) )
                          dd2=ifelse(dd==0,0,dd+(10-dd%%10) )
                          ee2=ifelse(ee==0,0,ee+(10-ee%%10) )
                          ff2=ifelse(ff==0,0,ff+(10-ff%%10) )
                          
                          which(aa2+bb2+cc2+dd2+ee2+ff2<=50)
                          
                          
                          #number of choices
                          choices=which(aa2+bb2+cc2+dd2+ee2+ff2<=50)
                          
                          #need to adjust the glove/chest/leg selection to match what they pick
                          
                          #find the right glove, etc.
                          one2=one[round(storage[which(aa2+bb2+cc2+dd2+ee2+ff2<=50),armorToSearch[1]]),]
                          two2=two[round(storage[which(aa2+bb2+cc2+dd2+ee2+ff2<=50),armorToSearch[2]]),]
                          three2=three[round(storage[which(aa2+bb2+cc2+dd2+ee2+ff2<=50),armorToSearch[3]]),]
                          
                        output$columnsGear = renderUI({
                                      #mydata = get(input$dataset)
                                      listNumText=ifelse(length(choices)<99,paste0(length(choices)),paste0('99+'))
                                      selectInput('columns9', paste('There are',listNumText,' that match your criteria: select below'), width=700,c(0: min(length(choices),99)  ) ,selected=TRUE)
                                    })
                        #  browser()
                          
                          observeEvent(input$go4,{
                          if (input$go4 == 0)   #this ingenious little bit of code stops the app from running prior to the first click of button
                                    return()
                            
                            
                                  output$value3=renderTable({
                                    
                                    
                                    
                                  observeEvent(input$columns9, {
                                    
                                   
                                  i=as.integer(as.character(input$columns9))
                                     })  
                                    i=as.integer(as.character(input$columns9))
                                    selected= storage[choices[i],]
                                    #paste(one[selected['Gauntlets'],],'\n',two[selected['Chest Armor'],],'\n',three[selected['Leg Armor'],])
                                #  outtie=rbind.data.frame(one[selected['Gauntlets'],],two[selected['Chest Armor'],],three[selected['Leg Armor'],])
                               outtie=rbind.data.frame(one2[i,],two2[i,],three2[i,])
                              
                                    colnames(outtie)=colnames(one)
                                  outtie$location[outtie$location==1]<-'equipped'
                                  outtie$location[outtie$location==2]<-'inventory'
                                  outtie$location[outtie$location==3]<-'vault'
                                #note location 1 =equipped, 2= inventory, 3= vault
                                  return(outtie)
                                  return(outtie)
                                    })
                                #  browser()
                               })#end go 4
                         # browser()
                          
                        
                              })#end go 3
                            
                            
                            
                            
                            
                        })#end go 2
             
             
             
             
             
             
             
             
             
        }) #end go 1
      
      
}#end server
        
        
  

        


shinyApp(ui = ui, server = server)
    
    
    
