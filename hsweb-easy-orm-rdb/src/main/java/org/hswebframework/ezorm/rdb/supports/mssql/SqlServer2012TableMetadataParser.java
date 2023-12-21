package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.commons.RDBTableMetadataParser;
import reactor.core.publisher.Flux;

import java.util.List;

/**
 * @author zhouhao
 */
public class SqlServer2012TableMetadataParser extends RDBTableMetadataParser {

    public SqlServer2012TableMetadataParser(RDBSchemaMetadata schema) {
        super(schema);
    }

    private static final String TABLE_META_SQL = String.join(
            " ",
            "SELECT",
            "    cols.TABLE_NAME as [table_name],",
            "    cols.COLUMN_NAME as [name],",
            "    cols.DATA_TYPE as [data_type],",
            "    cols.CHARACTER_MAXIMUM_LENGTH as [data_length],",
            "    cols.NUMERIC_PRECISION as [data_precision],",
            "    cols.NUMERIC_SCALE as [data_scale],",
            "    IIF(cols.IS_NULLABLE = 'NO', 1, 0) as [not_null],",
            "    IIF(cols.COLUMN_NAME IN (",
            "        SELECT COLUMN_NAME",
            "        FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE",
            "        WHERE TABLE_SCHEMA = cols.TABLE_SCHEMA",
            "          AND CONSTRAINT_NAME LIKE 'PK%'), 1, 0) as [primary_key],",
            "    cm.comment as [comment]",
            "FROM INFORMATION_SCHEMA.COLUMNS cols",
            "    LEFT JOIN (",
            "        SELECT OBJECT_NAME(ep.major_id) as [table_name],",
            "               col.name as [column_name],",
            "               cast(ep.value as nvarchar(500)) as [comment]",
            "        FROM sys.extended_properties ep",
            "            JOIN sys.columns col ON col.object_id = ep.major_id",
            "               AND col.column_id = ep.minor_id",
            "        WHERE ep.class = 1",
            "    ) cm ON cols.TABLE_NAME = cm.table_name",
            "          AND cols.COLUMN_NAME = cm.column_name",
            "WHERE cols.TABLE_SCHEMA = #{schema}",
            "  AND cols.TABLE_NAME LIKE #{table}");


    @Override
    protected String getTableMetaSql(String tname) {
        return TABLE_META_SQL;
    }

    @Override
    protected String getTableCommentSql(String tname) {
        return "select cast(ep.value as nvarchar(500)) as [comment],t.name as [table_name]" +
                " from sys.tables t" +
                " LEFT JOIN"+
                " sys.extended_properties ep ON t.object_id = ep.major_id AND ep.name = 'MS_Description'"+
                " where ep.minor_id=0 and t.schema_id = SCHEMA_ID(#{schema}) and t.name like #{table}";
    }

    @Override
    protected String getAllTableSql() {
        return "select name from sysobjects where xtype='U'";
    }

    @Override
    protected String getTableExistsSql() {
        return "select count(1) as total from sysobjects where xtype='U' and name = #{table}";
    }

    @Override
    public List<RDBTableMetadata> parseAll() {
        return super.fastParseAll();
    }

    @Override
    public Flux<RDBTableMetadata> parseAllReactive() {
        return super.fastParseAllReactive();
    }
}
