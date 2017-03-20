
##Dirty Information Table
drop table dwh.DIM_INFORMATION2_DIRTY;
create table dwh.DIM_INFORMATION2_DIRTY(
T_Id VARCHAR(20),
T_Name VARCHAR(200),
T_Position VARCHAR(200),
T_Office VARCHAR(200),
T_Age VARCHAR(200)
)
;
 
##Loading Data from the .CSV File
LOAD DATA LOCAL INFILE 
'/Users/Kalhan/Desktop/Waterloo Data/Winter 2017/BigData/Project/information2/dirty_information2.csv' 
INTO TABLE dwh.DIM_INFORMATION2_DIRTY FIELDS TERMINATED BY ',' 
ENCLOSED BY '"' LINES TERMINATED BY '\n';

##Setting Safe Mode Updates
SET SQL_SAFE_UPDATES = 0;

#Removing the header error
delete from dwh.DIM_INFORMATION2_DIRTY where T_Name = 'Name';



##CLEAN Information Table
drop table dwh.DIM_INFORMATION2_CLEAN;
create table dwh.DIM_INFORMATION2_CLEAN(
T_Id VARCHAR(20),
T_Name VARCHAR(200),
T_Position VARCHAR(200),
T_Office VARCHAR(200),
T_Age VARCHAR(200)
)
;


 
##Loading Data from the .CSV File
LOAD DATA LOCAL INFILE 
'/Users/Kalhan/Desktop/Waterloo Data/Winter 2017/BigData/Project/information2/clean_information2.csv' 
INTO TABLE dwh.DIM_INFORMATION2_CLEAN FIELDS TERMINATED BY ',' 
ENCLOSED BY '"' LINES TERMINATED BY '\n';

##Setting Safe Mode Updates
SET SQL_SAFE_UPDATES = 0;

#Removing the header error
delete from dwh.DIM_INFORMATION2_CLEAN where T_Name = 'Name';

#EDA on the two datasets to figure out the things that were actually cleaned

#Records Dirty
select count(*) from dwh.DIM_INFORMATION2_DIRTY;
#680

#Records Clean
select count(*) from dwh.DIM_INFORMATION2_CLEAN;
#680

drop table dwh.DIM_INFORMATION_DATA_TAG;
create table dwh.DIM_INFORMATION_DATA_TAG as(
select a.T_Id,
case when a.T_Age REGEXP '[0-9]+' Then 0 else 1 end as Age_Error,
case when a.T_Office REGEXP '[0-9]+' THEN 1 else 0 end as Office_Error,
case when a.T_Name in (select distinct T_Position from dwh.DIM_INFORMATION2_DIRTY ) THEN 1 else 0 end as Name_Error,
case when a.T_Position in (select distinct T_Name from dwh.DIM_INFORMATION2_DIRTY ) THEN 1 else 0 end as Position_Error,
case when ((a.T_id <> b.T_id) || (a.T_Name <> b.T_Name) || (a.T_Position <> b.T_Position) || (a.T_Age <> b.T_Age)) then '1' else '0' end as T_Class
from dwh.DIM_INFORMATION2_DIRTY as a 
join dwh.DIM_INFORMATION2_CLEAN as b on (a.T_id = b.T_id)
)
; 

select * from dwh.DIM_INFORMATION_DATA_TAG ;

select * from dwh.dim_information_data_tag where T_Position = 'Caesar Vance';
 




