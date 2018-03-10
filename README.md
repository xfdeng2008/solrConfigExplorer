# Solr Configurations Explorer
As your serach project goes on, the Solr configuration files, i.e., solrconfig.xml and schema.xml, become complicated and difficult to maintain (especially for new employees). There is a need to start an opensource project that visualizes the relationship between configurations and schemas interactively. 

Here is some senarios that you may need this tool. Your co-works or collegues just commited a code change. You may find difficulties in reviewing impacts of this change on your search application. A developing GUI with advanced search features may help. However, because of current Solr configuration settings, you might still need to search between the solrconfig.xml and schema.xml back and forth for several times. For example, think about how you can find which requestHandlers, fields and fieldtypes that will be impact by the change in a synonyms list? In this case, you need to search from bottom to top between the two xml files (synonyms files -> synonyms filters -> fieldTypes -> fields -> requestHandlers). 

In an opposite direction, suppose there are multiple synonyms lists and multiple fileds with multiple fieldTypes in your requestHandler. How can you find which the correct synonyms lists to maintain according to the requirements in your application? This time you may need to search from top to bottom (requestHandlers -> fields -> fieldsTypes -> synonyms filters -> synonyms files).

I started this simple project by trying to visualize relationship between Solr configurations. So far, the project only visualize simple relationship between requestHandlers in solrconfig.xml and type definitions in schema.xml

The project uses R programming language and R Shiny, please visit https://xiaofeideng.shinyapps.io/ExploreSolrConfigurations/ if interested.

The collapsible tree package in R is the main visulization tool for this project. Source of collapsible tree: https://github.com/AdeelK93/collapsibleTree

Your are more than welcome to add new features to this project, poingting out mistakes, and fixing bugs. 
