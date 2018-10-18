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

--
-- Convenience methods for creating unit tests
--



IF (OBJECT_ID('juts.generateValueForType') IS NOT NULL)
	DROP FUNCTION juts.generateValueForType;--
GO
CREATE FUNCTION juts.generateValueForType
(	@datatype	VARCHAR(MAX)
,	@offset		INT = 0
)
RETURNS VARCHAR(MAX)
AS
BEGIN
	RETURN CASE
		WHEN @datatype IN ('real','int','tinyint','smallint','money','float','decimal','numeric','smallmoney','bigint') THEN CAST((1 + @offset) AS VARCHAR(MAX))
		WHEN @datatype IN ('date','datetime','time','datetype2') THEN CONVERT(VARCHAR(MAX), DATEADD(DAY, @offset, '20180101 12:12:12.123'), 121)
		WHEN @datatype IN ('char','varchar','nvarchar','nchar') THEN CHAR(65 + @offset)
		ELSE NULL
	END
END
GO



IF (OBJECT_ID('juts.tableToCSV') IS NOT NULL)
	DROP PROCEDURE juts.tableToCSV;--
GO
CREATE PROCEDURE juts.tableToCSV
(	@database	VARCHAR(MAX) = NULL
,	@schema		VARCHAR(MAX) = NULL
,	@table		VARCHAR(MAX)
,	@data		VARCHAR(MAX) OUTPUT
)
AS
BEGIN
	DECLARE @csv VARCHAR(MAX)
	DECLARE @databaseWithDot VARCHAR(MAX) = ''
	DECLARE @fullName VARCHAR(MAX) = ''

	IF (@database IS NOT NULL)
		SET @databaseWithDot = @database + '.';--
	
	SET @fullName = @databaseWithDot

	IF (@schema IS NOT NULL)
		SET @fullName = @fullName + @schema + '.'
	
	SET @fullName = @fullName + @table

	DECLARE @sql NVARCHAR(MAX) = '
		SELECT
			@csv = SUBSTRING
			(
				(	SELECT
						'' + '''','''' + IIF('' + name + '' IS NULL, ''''NULL'''', ''''''''''''''''+CONVERT(VARCHAR(MAX), '' + name + '', 121)+'''''''''''''''')''
					FROM
						' + @databaseWithDot + 'sys.columns
					WHERE
						object_id = OBJECT_ID(''' + @fullName + ''')
					FOR XML PATH('''')
				)
				, 10
				, 200000
			)
	'
	EXEC sp_executesql @sql, N'@csv VARCHAR(MAX) OUTPUT', @csv OUTPUT

	SET @sql = 'SELECT @csv = @csv + CHAR(13) + CHAR(10) + ' + @csv + ' FROM ' + @fullName
	SET @csv = ''

	EXEC sp_executesql @sql, N'@csv VARCHAR(MAX) OUTPUT', @csv OUTPUT

	SELECT @data = SUBSTRING(@csv, 3, 20000000)
END
GO