/****** Script for SelectTopNRows command from SSMS  ******/
SELECT e.[Id]
	  ,[Level5ANZSCOCode]
      ,[NarrowFieldOfStudyCode]
	  ,[NZQALevel]
      ,e.[People]
      ,case [EthnicGroup]
		when 'European' then 1
		when 'Maori' then 2
		when 'Pacific' then 3
		when 'Asian' then 4
		when 'MELAA_Other' then 5
		when 'Total' then 77
		when 'Total_stated' then 76
	   end as EthnicGroup

      ,case [Sex] when 'Male' then 1  
	  when 'Female' then 2 
	  when 'Total' then 77 end as Sex
	into TestDb.dbo.ANZSCOOccupationL5NarrowFieldOfStudyAlternative
  FROM [GradOutcomesNZv4-1].[dbo].[ANZSCOOccupationL5NarrowFieldOfStudy] n 
  join [GradOutcomesNZv4-1].[dbo].[ANZSCOOccupationL5NarrowFieldOfStudyExtended] e
  on n.Id = e.OccupationFieldOfStudyId

  /*
  
Pacific                                 
Asian                                   
Maori                                   
MELAA_Other                             
Total                                   
European                                
Total_stated                            
  
  */