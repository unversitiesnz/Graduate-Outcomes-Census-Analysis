/****** Script for SelectTopNRows command from SSMS  ******/
insert into [GradOutcomesNZv5-0].[dbo].[ANZSCOOccupationL5]
SELECT [Level5ANZSCOCode]
      ,[Level4ANZSCOCode]
      ,[Level3ANZSCOCode]
      ,[Level2ANZSCOCode]
      ,[Level1ANZSCOCode]
      ,[ClassificationTitle]
      ,[TotalPeopleCount]
      ,case [TypicalStudyLevel]
		when 'School' then 1
		when 'Cert/Dipl' then 2
		when 'Mixed' then 3
		when 'Tertiary' then 4
		when 'Degree' then 5
	end as TypicalStudyLevelKey
  FROM [GradOutcomesNZv4-1].[dbo].[ANZSCOOccupationL5]
   where Level1ANZSCOCode != 0
 
