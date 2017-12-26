CREATE TABLE stg_area (
	Project	VARCHAR(100),
	Issue_Key	VARCHAR(20)	PRIMARY KEY,
	Summary	TEXT,
	Issue_Type	VARCHAR(100),
	Issue_Status	VARCHAR(100),
	Priority	VARCHAR(100),
	Resolution	VARCHAR(100),
	Assignee	VARCHAR(100),
	Reporter	VARCHAR(100),
	Creator	VARCHAR(100),
	Created	VARCHAR(100),
	Last_Viewed	VARCHAR(100),
	Updated	VARCHAR(100),
	Resolved	VARCHAR(100),
	Affect_Version_s	VARCHAR(100),
	Fix_Version_s	VARCHAR(100),
	Component_s	VARCHAR(100),
	Due_Date	VARCHAR(100),
	Votes	INT,
	Watchers	INT,
	Images	TEXT,
	Original_Estimate	INT,
	Remaining_Estimate	INT,
	Time_Spent	INT,
	Work_Ratio	VARCHAR(100),
	Sub_tasks	VARCHAR(100),
	Linked_Issues	VARCHAR(100),
	Environment	TEXT,
	Description	TEXT,
	Security_Level	VARCHAR(100),
	Progress	VARCHAR(100),
	Σ_Progress	VARCHAR(100),
	Σ_Time_Spent	INT,
	Σ_Remaining_Estimate	INT,
	Σ_Original_Estimate	INT,
	Labels	VARCHAR(100),
	Flagged	VARCHAR(100),
	Epic_Theme	VARCHAR(100),
	Test_Session_s	VARCHAR(100),
	Raised_During	VARCHAR(100),
	Sprint	VARCHAR(100),
	Story_Points	VARCHAR(100),
	Business_Value	VARCHAR(100),
	Epic_Link	VARCHAR(100),
	Testing_Status	VARCHAR(100),
	Epic_Name	VARCHAR(100),
	Rank	VARCHAR(100),
	Ticket_cliente	INT,
	Follow_Status	VARCHAR(100),
	Release_Version_History	VARCHAR(100),
	Solicitante	VARCHAR(100),
	Development	VARCHAR(100),
	Parent_Link	VARCHAR(100),
	CHART_Date_of_First_Response	VARCHAR(100),
	Epic_Status	VARCHAR(100),
	Team	VARCHAR(100),
	Epic_Colour	VARCHAR(100),
	Organizations	VARCHAR(100)
);