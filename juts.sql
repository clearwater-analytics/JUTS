/**********************************************************************************
 *                      Jason's Unit Testing for SQL
 *                            J . U . T . S .
 *              Sets up a simple unit testing framework for SQL
 **********************************************************************************
 * Copyright (c) 2018 Clearwater Analytics
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
 * Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 **********************************************************************************/



IF NOT EXISTS (SELECT * FROM sys.schemas WHERE name = 'juts')
	EXEC('CREATE SCHEMA juts AUTHORIZATION dbo')
GO



IF (OBJECT_ID('juts.Scenario') IS NOT NULL)
	DROP TABLE juts.Scenario;--
GO
CREATE TABLE juts.Scenario
(	scenarioId							INT IDENTITY(1,1) PRIMARY KEY
,	name								VARCHAR(MAX)
,	comment								VARCHAR(MAX)
)



IF (OBJECT_ID('juts.command') IS NOT NULL)
	DROP TABLE juts.command;--
GO
CREATE TABLE juts.command
(	commandId							INT IDENTITY(1,1) PRIMARY KEY
,	scenarioId							INT
,	command								NVARCHAR(MAX)
,	initializationCode					NVARCHAR(MAX)
,	expectedReturnValue					VARCHAR(MAX)
,	expectedResultSetColumnsAndTypes	VARCHAR(MAX)
,	expectedResultSetCSV				VARCHAR(MAX)
,	ignoreColumnsFromResultSet			VARCHAR(MAX)
,	expectedException					VARCHAR(MAX)
,	columnOrder							VARCHAR(MAX)
)



IF (OBJECT_ID('juts.trackTable') IS NOT NULL)
	DROP TABLE juts.trackTable;--
GO
CREATE TABLE juts.trackTable
(	id					INT IDENTITY(1,1) PRIMARY KEY
,	scenarioId			INT
,	tableSchema			VARCHAR(MAX)
,	tableName			VARCHAR(MAX)
,	initialValueCSV		VARCHAR(MAX)
,	expectedValueCSV	VARCHAR(MAX)
,	columnOrder			VARCHAR(MAX)
,	ignoreColumns		VARCHAR(MAX)
,	ignoreConstraints	BIT
,	ignoreInsertColumns	VARCHAR(MAX)
)



IF (OBJECT_ID('juts.failures') IS NOT NULL)
	DROP TABLE juts.failures;--
GO
CREATE TABLE juts.failures
(	scenarioId	BIGINT
,	name		VARCHAR(MAX)
,	comment		VARCHAR(MAX)
,	schemaName	VARCHAR(MAX)
,	tableName	VARCHAR(MAX)
,	expected	VARCHAR(MAX)
,	actual		VARCHAR(MAX)
)



IF (OBJECT_ID('juts.CreateTestScenario') IS NOT NULL)
	DROP PROCEDURE juts.CreateTestScenario;--
GO
CREATE PROCEDURE juts.CreateTestScenario
(	@name									VARCHAR(MAX)
,	@comment								VARCHAR(MAX) = NULL
)
AS
	INSERT INTO juts.Scenario
	(	name
	,	comment
	)
	VALUES
	(	@name
	,	@comment
	)
	RETURN @@IDENTITY
GO



IF (OBJECT_ID('juts.AddCommandToScenario') IS NOT NULL)
	DROP PROCEDURE juts.AddCommandToScenario;--
GO
CREATE PROCEDURE juts.AddCommandToScenario
(	@scenarioId								INT
,	@command								NVARCHAR(MAX)
,	@initializationCode						NVARCHAR(MAX) = NULL
,	@expectedReturnValue					VARCHAR(MAX) = NULL
,	@expectedResultSetColumnsAndTypes		VARCHAR(MAX) = NULL
,	@expectedResultSetCSV					VARCHAR(MAX) = NULL
,	@ignoreColumnsFromResultSet				VARCHAR(MAX) = NULL
,	@expectedException						VARCHAR(MAX) = NULL
,	@resultsColumnOrderBy					VARCHAR(MAX) = NULL
)
AS
	IF ((@expectedResultSetColumnsAndTypes IS NULL OR @expectedResultSetCSV IS NULL) AND (@expectedResultSetColumnsAndTypes IS NOT NULL OR @expectedResultSetCSV IS NOT NULL OR @ignoreColumnsFromResultSet IS NOT NULL))
	BEGIN
		throw 70551, 'If any of @expectedSelectReturnColumnsAndTypes, @expectedSelectCSV, or @ignoreColumns are specified, @expectedSelectReturnColumnsAndTypes and @expectedSelectCSV must be specified', 1;--
	END

	INSERT INTO juts.command
	(	scenarioId
	,	command
	,	initializationCode
	,	expectedReturnValue
	,	expectedResultSetColumnsAndTypes
	,	expectedResultSetCSV
	,	ignoreColumnsFromResultSet
	,	expectedException
	,	columnOrder
	)
	VALUES
	(	@scenarioId
	,	@command
	,	@initializationCode
	,	@expectedReturnValue
	,	@expectedResultSetColumnsAndTypes
	,	@expectedResultSetCSV
	,	@ignoreColumnsFromResultSet
	,	@expectedException
	,	@resultsColumnOrderBy
	)
	RETURN @@IDENTITY
GO



IF (OBJECT_ID('juts.AddTableToScenario') IS NOT NULL)
	DROP PROCEDURE juts.AddTableToScenario;--
GO
CREATE PROCEDURE juts.AddTableToScenario
(	@scenarioId				INT
,	@tableSchema			VARCHAR(MAX)
,	@tableName				VARCHAR(MAX)
,	@initialValueCSV		VARCHAR(MAX) = NULL
,	@expectedValueCSV		VARCHAR(MAX) = NULL
,	@resultsColumnOrderBy	VARCHAR(MAX) = NULL
,	@ignoreColumns			VARCHAR(MAX) = NULL
,	@ignoreConstraints		BIT = 0
,	@ignoreInsertColumns	VARCHAR(MAX) = NULL
)
AS
	INSERT INTO juts.trackTable
	(	scenarioId
	,	tableSchema
	,	tableName
	,	initialValueCSV
	,	expectedValueCSV
	,	columnOrder
	,	ignoreColumns
	,	ignoreConstraints
	,	ignoreInsertColumns
	)
	VALUES
	(	@scenarioId
	,	@tableSchema
	,	@tableName
	,	@initialValueCSV
	,	@expectedValueCSV
	,	@resultsColumnOrderBy
	,	@ignoreColumns
	,	@ignoreConstraints
	,	@ignoreInsertColumns
	)
GO



IF (OBJECT_ID('juts.getColumnsAsPattern') IS NOT NULL)
	DROP FUNCTION juts.getColumnsAsPattern;--
GO
CREATE FUNCTION juts.getColumnsAsPattern
(		@schema			VARCHAR(MAX)
	,	@table			VARCHAR(MAX)
	,	@pattern		VARCHAR(MAX)
	,	@ignoreColumns	VARCHAR(MAX) = NULL
)
RETURNS VARCHAR(MAX)
AS
BEGIN
	IF OBJECT_ID(@schema + '.' + @table) IS NULL
	BEGIN
		RETURN ''
	END

	DECLARE @return VARCHAR(MAX)
	SELECT
		@return =
			(	SELECT
					REPLACE(@pattern, '$$COLUMN$$', COLUMN_NAME)
				FROM
					information_schema.columns c2
				WHERE
						c1.TABLE_NAME = c2.TABLE_NAME
					AND	c1.TABLE_SCHEMA = c2.TABLE_SCHEMA
					AND	(@ignoreColumns IS NULL OR COLUMN_NAME NOT IN (SELECT Token FROM split(@ignoreColumns, ',')))
				FOR XML PATH('')
			)
	FROM
		information_schema.columns c1
	WHERE
			TABLE_SCHEMA = @schema
		AND	TABLE_NAME = @table
	GROUP BY
		TABLE_NAME, TABLE_SCHEMA
	RETURN @return
END
GO



IF (OBJECT_ID('juts.getColumnsAsCSV') IS NOT NULL)
	DROP FUNCTION juts.getColumnsAsCSV;--
GO
CREATE FUNCTION juts.getColumnsAsCSV
(		@schema			VARCHAR(MAX)
	,	@table			VARCHAR(MAX)
	,	@ignoreColumns	VARCHAR(MAX) = NULL
)
RETURNS VARCHAR(MAX)
AS
BEGIN
	IF OBJECT_ID(@schema + '.' + @table) IS NULL
	BEGIN
		RETURN ''
	END

	DECLARE @return VARCHAR(MAX)
	SELECT @return = STUFF(juts.getColumnsAsPattern(@schema, @table, ',$$COLUMN$$', @ignoreColumns), 1, 1, '')
	RETURN @return
END
GO



IF (OBJECT_ID('juts.getTableValuesAsCSV') IS NOT NULL)
	DROP PROCEDURE juts.getTableValuesAsCSV;--
GO
CREATE PROCEDURE juts.getTableValuesAsCSV
(		@schema			VARCHAR(MAX)
	,	@table			VARCHAR(MAX)
	,	@csv			VARCHAR(MAX) OUTPUT
	,	@resultsOrderBy	VARCHAR(MAX) = NULL
	,	@ignoreColumns	VARCHAR(MAX) = NULL
)
AS
BEGIN
	IF OBJECT_ID(@schema + '.' + @table) IS NULL
	BEGIN
		THROW 71184, 'The table does not exist', 1
	END

	DECLARE @sql NVARCHAR(MAX)
	SELECT
		@sql = '
		SELECT
			@csv = IIF(@csv IS NULL, '''', @csv + char(13) + CHAR(10)) +
				REVERSE(
					STUFF(
						REVERSE(
							CONCAT(' + juts.getColumnsAsPattern(@schema, @table, 'IIF($$COLUMN$$ IS NULL,''NULL'', '''''''' + CONVERT(VARCHAR(MAX), $$COLUMN$$, 121) + ''''''''), '','', ', @ignoreColumns) + ''''')
						)
					, 1,1,'''')
				)
		FROM
			' + @schema + '.' + @table

	IF (@resultsOrderBy IS NOT NULL)
		SET @sql = @sql + ' ORDER BY ' + @resultsOrderBy

	EXEC sp_executesql @sql, N'@csv VARCHAR(MAX) OUTPUT', @csv OUTPUT
	SELECT @csv = @csv
END
GO



IF (OBJECT_ID('juts.truncateTableAndPopulateFromCSV') IS NOT NULL)
	DROP PROCEDURE juts.truncateTableAndPopulateFromCSV;--
GO
CREATE PROCEDURE juts.truncateTableAndPopulateFromCSV
(		@schema					VARCHAR(MAX)
	,	@table					VARCHAR(MAX)
	,	@csv					VARCHAR(MAX)
	,	@ignoreInsertColumns	VARCHAR(MAX)
)
AS
BEGIN
	IF (@@TRANCOUNT <= 0)
	BEGIN
		THROW 334455, 'You must wrap this call in a transaction', 1;--
	END

	IF OBJECT_ID(@schema + '.' + @table) IS NULL
	BEGIN
		THROW 71184, 'The table does not exist', 1
	END

	DECLARE @parameters VARCHAR(MAX) = juts.getColumnsAsCSV(@schema, @table, @ignoreInsertColumns)
	DECLARE @sql VARCHAR(MAX)

	SELECT @sql = '
		SELECT TOP 0 ' + @parameters + ' INTO #temp FROM ' + @schema + '.' + @table + ' UNION ALL SELECT ' + @parameters + ' FROM ' + @schema + '.' + @table + ' WHERE 1 = 0
		TRUNCATE TABLE ' + @schema + '.' + @table + '
		EXEC master..sp_InsertFromCsv @tableName = ''#temp'', @csv = ''' + REPLACE(@csv,'''','''''') + '''
		IF EXISTS (SELECT 1 FROM sys.columns c WHERE c.object_id = object_id(''' + @schema + '.' + @table + ''') AND c.is_identity = 1)
			SET IDENTITY_INSERT ' + @schema + '.' + @table + ' ON
		INSERT INTO ' + @schema + '.' + @table + ' (' + @parameters + ') SELECT ' + @parameters + ' FROM #temp
		IF EXISTS (SELECT 1 FROM sys.columns c WHERE c.object_id = object_id(''' + @schema + '.' + @table + ''') AND c.is_identity = 1)
			SET IDENTITY_INSERT ' + @schema + '.' + @table + ' OFF
		DROP TABLE #temp
	'

	EXEC (@sql)
END
GO


IF (OBJECT_ID('juts.getForeignKeyDefinitions') IS NOT NULL)
	DROP FUNCTION juts.getForeignKeyDefinitions;--
GO
CREATE FUNCTION juts.getForeignKeyDefinitions
(	@scenarioId	INT
)
RETURNS TABLE
AS
RETURN
	WITH X AS
	(	SELECT
				f.name AS [constraint]
			,	SCHEMA_NAME(kt.schema_id) AS [schema]
			,	(kt.name) AS [table]
			,	COL_NAME(fc.parent_object_id,fc.parent_column_id) AS [column]
			,	SCHEMA_NAME(t.schema_id) AS [reference_schema]
			,	OBJECT_NAME(t.object_id) AS [reference_table]
			,	COL_NAME(t.object_id,fc.referenced_column_id) [reference_column]
			,	f.delete_referential_action_desc
			,	f.update_referential_action_desc
			,	tt.ignoreConstraints
		FROM
			juts.trackTable tt
			JOIN sys.tables t
				ON	OBJECT_NAME(t.object_id) = tt.tableName
				AND	SCHEMA_NAME(t.schema_id) = tt.tableSchema
			JOIN sys.foreign_key_columns AS fc
				ON	t.OBJECT_ID = fc.referenced_object_id
			JOIN sys.foreign_keys AS f
				ON	f.OBJECT_ID = fc.constraint_object_id
			JOIN sys.tables kt
				ON	f.parent_object_id = kt.object_id
		WHERE
				tt.initialValueCSV IS NOT NULL
			AND	tt.scenarioId = @scenarioId
	)
	SELECT
			X.[constraint]
		,	X.[schema] 
		,	X.[table]
		,	STUFF( (SELECT	', [' + X1.[column] + ']'
					FROM	X X1
					WHERE	X1.[constraint] = X.[constraint] 
							AND X1.[schema] = X.[schema]
							AND X1.[table] = X.[table] FOR XML PATH(''), 
					TYPE).value('.[1]', 'nvarchar(max)'), 1, 2, '') AS [columns]
		,	X.[reference_schema]
		,	X.[reference_table]
		,	STUFF( (SELECT	', [' + X1.[reference_column] + ']'
					FROM	X X1
					WHERE	X1.[constraint] = X.[constraint]
							AND X1.[schema] = X.[schema]
							AND X1.[table] = X.[table] FOR XML PATH(''), 
					TYPE).value('.[1]', 'nvarchar(max)'), 1, 2, '') AS [reference_columns]
		,	X.delete_referential_action_desc
		,	X.update_referential_action_desc
		,	X.ignoreConstraints
	FROM
		X
	GROUP BY
			X.[constraint]
		,	X.[schema]
		,	X.[table]
		,	X.[reference_schema]
		,	X.[reference_table]
		,	X.delete_referential_action_desc
		,	X.update_referential_action_desc
		,	X.ignoreConstraints
GO



IF (OBJECT_ID('juts.populateTablesForScenario') IS NOT NULL)
	DROP PROCEDURE juts.populateTablesForScenario;--
GO
CREATE PROCEDURE juts.populateTablesForScenario
(	@scenarioId	INT
)
AS
	IF (@@TRANCOUNT <= 0)
	BEGIN
		THROW 334455, 'You must wrap this call in a transaction', 1;--
	END

	SELECT
			ROW_NUMBER() OVER (ORDER BY [delete] ASC) as row
		,	*
	INTO
		#foreignKeys
	FROM
	(	SELECT
				'ALTER TABLE [' + fkd.[schema] + '].[' + fkd.[table]  + '] DROP CONSTRAINT [' + fkd.[constraint] + ']' AS [delete]
			,	'ALTER TABLE [' + fkd.[schema] + '].[' + fkd.[table]  + ']' + IIF(fkd.ignoreConstraints = 1, 'WITH NOCHECK', '') + ' ADD CONSTRAINT [' +  fkd.[constraint] + '] '
				+ 'FOREIGN KEY(' + fkd.[columns] + ')'
				+ ' REFERENCES ' +  '[' + fkd.[reference_schema] + '].[' + fkd.[reference_table] + '] (' + fkd.[reference_columns] + ') '
				+ IIF(fkd.delete_referential_action_desc <> 'NO_ACTION', 'ON DELETE ' + CAST(fkd.delete_referential_action_desc AS VARCHAR(20)) COLLATE Latin1_General_CI_AS, '')
				+ IIF(fkd.update_referential_action_desc <> 'NO_ACTION', ' ON UPDATE ' + CAST(fkd.update_referential_action_desc AS VARCHAR(20)) COLLATE Latin1_General_CI_AS, '')
			   AS [create]
		FROM
			juts.getForeignKeyDefinitions(@scenarioId) fkd
	) x

	-- Drop foreign keys
	DECLARE @i INT = 0
	DECLARE @delete VARCHAR(MAX)
	WHILE EXISTS (SELECT 1 FROM #foreignKeys WHERE @i < row)
	BEGIN
		SELECT TOP 1
				@i = row
			,	@delete = [delete]
		FROM
			#foreignKeys
		WHERE
			@i < row
		ORDER BY
			row ASC
		EXEC(@delete)
	END

	-- Put the data into the tables
	DECLARE @tableId				INT = 0
	DECLARE @tableSchema			VARCHAR(MAX)
	DECLARE @tableName				VARCHAR(MAX)
	DECLARE @initialValueCSV		VARCHAR(MAX)
	DECLARE @ignoringInsertColumns	VARCHAR(MAX)
	DECLARE @ignoreConstraints		BIT
	WHILE EXISTS (SELECT 1 FROM juts.trackTable WHERE scenarioId = @scenarioId AND id > @tableId AND initialValueCSV IS NOT NULL)
	BEGIN
		SELECT TOP 1
				@tableId = id
			,	@tableSchema = tableSchema
			,	@tableName = tableName
			,	@initialValueCSV = initialValueCSV
			,	@ignoringInsertColumns = ignoreInsertColumns
			,	@ignoreConstraints = ignoreConstraints
		FROM
			juts.trackTable
		WHERE
				scenarioId = @scenarioId
			AND	id > @tableId
			AND	initialValueCSV IS NOT NULL
		ORDER BY
			id ASC

		-- Ignore constraints if needed
		IF (@ignoreConstraints = 1)
			EXEC ('ALTER TABLE ' + @tableSchema + '.' + @tableName + ' NOCHECK CONSTRAINT all')
		
		EXEC juts.truncateTableAndPopulateFromCSV @schema = @tableSchema, @table = @tableName, @csv = @initialValueCSV, @ignoreInsertColumns = @ignoringInsertColumns
		
		-- Stop ignoring constraints, but don't check existing values
		IF (@ignoreConstraints = 1)
			EXEC ('ALTER TABLE ' + @tableSchema + '.' + @tableName + ' WITH NOCHECK CHECK CONSTRAINT all')
	END

	-- Add back the foreign keys
	SET @i = 0
	DECLARE @create VARCHAR(MAX)
	WHILE EXISTS (SELECT 1 FROM #foreignKeys WHERE @i < row)
	BEGIN
		SELECT TOP 1
				@i = row
			,	@create = [create]
		FROM
			#foreignKeys
		WHERE
			@i < row
		ORDER BY
			row ASC

		BEGIN TRY
			EXEC(@create)
		END TRY
		BEGIN CATCH
			DECLARE @msg VARCHAR(MAX) = 'JUTS Could not re-create the following foreign key. Is the initial test data consistent with foreign key constraints?' + CHAR(10) + @create
			;--
			THROW 51079, @msg, 0
		END CATCH
	END
GO



IF (OBJECT_ID('juts.checkTablesForScenario') IS NOT NULL)
	DROP PROCEDURE juts.checkTablesForScenario;--
GO
CREATE PROCEDURE juts.checkTablesForScenario
(	@scenarioId	INT
)
AS
	DECLARE @tableId		INT = 0
	DECLARE @nextTableId	INT = 0
	DECLARE @tableSchema	VARCHAR(MAX)
	DECLARE @tableName		VARCHAR(MAX)
	DECLARE @expectedValue	VARCHAR(MAX)
	DECLARE @columnOrder	VARCHAR(MAX)
	DECLARE @ignoreColumns	VARCHAR(MAX)
	DECLARE @actualValue	VARCHAR(MAX)

	CREATE TABLE #temp
	(	schemaName	VARCHAR(MAX)
	,	tableName	VARCHAR(MAX)
	,	expected	VARCHAR(MAX)
	,	actual		VARCHAR(MAX)
	)

	WHILE EXISTS (SELECT 1 FROM juts.trackTable WHERE scenarioId = @scenarioId AND id > @tableId AND expectedValueCSV IS NOT NULL)
	BEGIN

		SELECT TOP 1
				@nextTableId = id
			,	@tableSchema = tableSchema
			,	@tableName = tableName
			,	@expectedValue = expectedValueCSV
			,	@columnOrder = columnOrder
			,	@ignoreColumns = ignoreColumns
		FROM
			juts.trackTable
		WHERE
				scenarioId = @scenarioId
			AND	id > @tableId
			AND	expectedValueCSV IS NOT NULL
		ORDER BY
			id ASC
	
		SET @actualValue = NULL
		EXEC juts.getTableValuesAsCSV @schema = @tableSchema, @table = @tableName, @resultsOrderBy = @columnOrder, @ignoreColumns = @ignoreColumns, @csv = @actualValue OUTPUT

		IF (@actualValue IS NULL)
			SET @actualValue = ''


		INSERT INTO #temp (schemaName, tableName, expected, actual)
		SELECT
				tableSchema
			,	tableName
			,	expectedValueCSV
			,	@actualValue
		FROM
			juts.trackTable
		WHERE
				scenarioId = @scenarioId
			AND	tableSchema = @tableSchema
			AND	tableName = @tableName
			AND	expectedValueCSV <> @actualValue

		SET @tableId = @nextTableId
	END

	SELECT * FROM #temp
GO



IF (OBJECT_ID('juts.ResetState') IS NOT NULL)
	DROP PROCEDURE juts.ResetState;--
GO
CREATE PROCEDURE juts.ResetState
AS
	TRUNCATE TABLE juts.trackTable
	TRUNCATE TABLE juts.command
	TRUNCATE TABLE juts.Scenario
GO



IF (OBJECT_ID('juts.RunScenarios') IS NOT NULL)
	DROP PROCEDURE juts.RunScenarios;--
GO
CREATE PROCEDURE juts.RunScenarios
AS
	-- So it rolls back on exception
	SET XACT_ABORT ON

	DECLARE @startedWithTransaction INT = @@TRANCOUNT

	-- Ensure there is an outer transaction
	-- This allows manual running as well as liquibase runs
	DECLARE @startedTransaction BIT = 0

	-- Variables for running the tests
	DECLARE @testId			INT = 1	-- current test index
	-- name of temp table for this session (allows more than one session to exec at the same time)
	DECLARE @tempTable		VARCHAR(MAX) = 'results'+CONVERT(VARCHAR(MAX),@@spid)

	DECLARE @oldFailures TABLE
	(	scenarioId	BIGINT
	,	name		VARCHAR(MAX)
	,	comment		VARCHAR(MAX)
	,	schemaName	VARCHAR(MAX)
	,	tableName	VARCHAR(MAX)
	,	expected	VARCHAR(MAX)
	,	actual		VARCHAR(MAX)
	)

	INSERT INTO @oldFailures
	SELECT * FROM juts.failures

	DECLARE @allFailures TABLE
	(	scenarioId	BIGINT
	,	name		VARCHAR(MAX)
	,	comment		VARCHAR(MAX)
	,	schemaName	VARCHAR(MAX)
	,	tableName	VARCHAR(MAX)
	,	expected	VARCHAR(MAX)
	,	actual		VARCHAR(MAX)
	)

	WHILE @testId <= (SELECT COUNT(1) FROM juts.Scenario)
	BEGIN
		-- Ensure there is an outer transaction
		-- This allows manual running as well as liquibase runs
		IF @@trancount = 0
		BEGIN
			BEGIN TRANSACTION
			SET @startedTransaction = 1
		END
		SAVE TRANSACTION juts_unit_test -- So it can roll back without wiping out actual data

		BEGIN TRY
			DECLARE @errored								BIT           = 0
			DECLARE @unexpectedErrorThrown					BIT           = 0
			DECLARE @command								NVARCHAR(MAX) = NULL
			DECLARE @initializationCode						NVARCHAR(MAX) = NULL
			DECLARE @commandOutput							VARCHAR(MAX) = NULL
			DECLARE @actualReturnValue						VARCHAR(MAX) = NULL
			DECLARE @expectedReturnValue					VARCHAR(MAX) = NULL
			DECLARE @expectedResultSetColumnsAndTypes		VARCHAR(MAX) = NULL
			DECLARE @expectedException						VARCHAR(MAX) = NULL
			DECLARE @actualResultSetCSV						VARCHAR(MAX) = NULL
			DECLARE @ignoreColumns							VARCHAR(MAX) = NULL
			DECLARE @resultsColumnOrderBy					VARCHAR(MAX) = NULL
			DECLARE @currentFailures TABLE
			(	schemaName	VARCHAR(MAX)
			,	tableName	VARCHAR(MAX)
			,	expected	VARCHAR(MAX)
			,	actual		VARCHAR(MAX)
			)

			PRINT 'Running test ' + CAST(@testId as VARCHAR)

			EXEC juts.populateTablesForScenario @scenarioId = @testId

			DECLARE @commandId INT = 0
			WHILE EXISTS (SELECT 1 FROM juts.command WHERE scenarioId = @testId AND commandId > @commandId AND @commandId IS NOT NULL)
			BEGIN
				SELECT TOP 1
						@commandId = commandId
					,	@command = command
					,	@initializationCode = initializationCode
					,	@expectedReturnValue = expectedReturnValue
					,	@expectedResultSetColumnsAndTypes = expectedResultSetColumnsAndTypes
					,	@expectedException = expectedException
					,	@ignoreColumns = ignoreColumnsFromResultSet
					,	@resultsColumnOrderBy = columnOrder
				FROM
					juts.command
				WHERE
						scenarioId = @testId
					AND	commandId > @commandId
				ORDER BY
					commandId ASC

				IF (@expectedResultSetColumnsAndTypes IS NOT NULL)
				BEGIN
					SET @actualResultSetCSV = NULL

					-- create dynamic table
					IF (OBJECT_ID('juts.'+@tempTable) IS NOT NULL) EXEC('DROP TABLE juts.'+@tempTable)
					EXEC('CREATE TABLE juts.'+@tempTable+'('+@expectedResultSetColumnsAndTypes+')')
					-- modify command to populate table
					SET @command = 'INSERT INTO juts.' + @tempTable + ' ' + @command
					IF (@initializationCode IS NOT NULL)
						SET @command = @initializationCode + ';' + @command
					EXEC sp_executesql @command, N'@commandOutput VARCHAR(MAX) OUTPUT', @commandOutput OUTPUT
					-- check the result
					EXEC juts.getTableValuesAsCSV @schema = 'juts', @table = @tempTable, @ignoreColumns = @ignoreColumns, @resultsOrderBy = @resultsColumnOrderBy, @csv = @actualResultSetCSV OUTPUT
					
					-- If there was nothing, convert to an empty string for the representation of an empty set
					IF (@actualResultSetCSV IS NULL)
						SET @actualResultSetCSV = ''

					INSERT INTO @currentFailures
					SELECT
							'N/A'
						,	'Command ResultSet'
						,	expectedResultSetCSV
						,	@actualResultSetCSV
					FROM
						juts.command
					WHERE
							scenarioId = @testId
						AND	commandId = @commandId
						AND
						(		expectedResultSetCSV <> @actualResultSetCSV
							OR	expectedResultSetCSV IS NULL AND @actualResultSetCSV IS NOT NULL
							OR	expectedResultSetCSV IS NOT NULL AND @actualResultSetCSV IS NULL
						)
				END
				ELSE
				BEGIN
					IF (@initializationCode IS NOT NULL)
						SET @command = @initializationCode + ';' + @command

					IF (@expectedReturnValue IS NOT NULL)
						EXEC sp_executesql @command, N'@commandOutput VARCHAR(MAX) OUTPUT', @commandOutput OUTPUT
					ELSE
						EXEC sp_executesql @command
				END

				IF (@expectedReturnValue IS NOT NULL)
				BEGIN
					INSERT INTO @currentFailures
					SELECT
							'N/A'
						,	'Scenario return output'
						,	expectedReturnValue
						,	@actualReturnValue
					FROM
						juts.command
					WHERE
							scenarioId = @testId
						AND	expectedReturnValue <> @actualReturnValue
				END
			END
			
			INSERT INTO @currentFailures
			EXEC juts.checkTablesForScenario @scenarioId = @testId

			ROLLBACK TRANSACTION juts_unit_test
		END TRY
		BEGIN CATCH
			SET @errored = 1
			ROLLBACK TRANSACTION
			IF (@expectedException IS NULL)
			BEGIN
				SET @unexpectedErrorThrown = 1
				PRINT '- !!!!!!!!!!!!!!!!!'
				PRINT '- !!!!! ERROR !!!!!'
				PRINT '- !!!!!!!!!!!!!!!!!'
				PRINT '- An unexpected exception occurred'
				PRINT ERROR_MESSAGE()
				INSERT INTO @currentFailures VALUES ('ERROR','ERROR','',ERROR_MESSAGE())
			END
			ELSE IF (ERROR_MESSAGE() <> @expectedException)
			BEGIN
				INSERT INTO @currentFailures VALUES ('N/A','Wrong exception thrown',@expectedException,ERROR_MESSAGE())
			END
		END CATCH

		IF (@expectedException IS NOT NULL AND @errored = 0)
		BEGIN
			INSERT INTO @currentFailures VALUES ('N/A','Exception expected but not thrown',@expectedException,NULL)
		END

		IF EXISTS (SELECT 1 FROM @currentFailures)
		BEGIN
			IF @unexpectedErrorThrown = 0
			BEGIN
				PRINT '- !!!!!!!!!!!!!!!!!!!!!!!'
				PRINT '- !!!!! Test failed !!!!!'
				PRINT '- !!!!!!!!!!!!!!!!!!!!!!!'
			END
			INSERT INTO @allFailures
			SELECT scenarioId, name, comment, schemaName, tableName, expected, actual FROM juts.Scenario CROSS APPLY @currentFailures WHERE scenarioId = @testId
		END
		ELSE IF @unexpectedErrorThrown = 0
			PRINT '- passed'

		DELETE FROM @currentFailures

		-- Move on to the next test
		PRINT 'Finished test ' + CAST(@testId as VARCHAR)
		SET @testId = @testId + 1
	END

	IF @startedTransaction = 1 AND @errored = 0
	BEGIN
		ROLLBACK TRANSACTION
	END

	EXEC juts.ResetState

	DELETE FROM juts.failures

	INSERT INTO juts.failures
	SELECT * FROM @oldFailures

	IF EXISTS (SELECT 1 FROM @allFailures)
	BEGIN
		INSERT INTO juts.failures
		SELECT * FROM @allFailures
		
		DECLARE @msg VARCHAR(MAX)
		SELECT @msg = 'There were ' +  CONVERT(VARCHAR(MAX), COUNT(1)) + ' unit test failures' FROM @allFailures;--
		
		IF @startedWithTransaction = 0
		BEGIN
			SELECT * FROM juts.failures;--
			THROW 70043, @msg, 1
		END
		ELSE
			PRINT @msg
	END

	-- If we started with a transaction but we didn't end with one, create a new one
	-- this is to appease MSSQL, which doesn't like when you wrap a proc in a transaction and roll it back in the proc
	IF @startedWithTransaction <> 0 AND @@TRANCOUNT = 0
		BEGIN TRANSACTION

	PRINT '***'
	PRINT 'FINISHED EXECUTION'
GO