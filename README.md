# JUTS : Unit Testing for SQL

JUTS is a SQL unit testing framework written in T-SQL.

Tables can be set to an initial state, functions and procedures can be executed, and results can be verified at the end.

A simple example of a unit test in JUTS is shown below. This will call a stored procedure and verify the output.

```
DECLARE @scenarioId INT

EXEC @scenarioId = juts.CreateTestScenario
    @name = 'Simple Insert'
,   @comment = 'Ensure we can insert into an empty table'

EXEC juts.addCommandToScenario
    @scenarioId = @scenarioId
,   @command = 'EXEC dbo.TestInsertStuff @key = ''T'', @values = ''A,B,C'''
,   @expectedReturnValue = NULL
,   @expectedResultSetColumnsAndTypes = 'keyId INT, value VARCHAR(100)'
,   @expectedResultSetCSV = '''1'',''A''
''1'',''B''
''1'',''C'''

EXEC juts.RunScenarios
```