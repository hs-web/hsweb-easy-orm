package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.RecordResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.parser.IndexMetadataParser;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServer2012TableMetadataParser;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Map;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.template;

/** Uses SQL Server compatibility views and retains the native sync/reactive parsing flow. */
public class KingbaseSqlServerTableMetadataParser extends SqlServer2012TableMetadataParser {

    private static final String TABLE_META_SQL = String.join(" ",
        "SELECT cols.TABLE_NAME as [table_name], cols.COLUMN_NAME as [name],",
        "cols.DATA_TYPE as [data_type], CASE WHEN format_type(a.atttypid, a.atttypmod) LIKE '%(max)'",
        "THEN format_type(a.atttypid, a.atttypmod) ELSE cols.DATA_TYPE END as [column_type],",
        "COALESCE(cols.CHARACTER_MAXIMUM_LENGTH, CASE WHEN cols.DATA_TYPE IN ('nvarchar','nchar')",
        "THEN c.max_length / 2 ELSE c.max_length END) as [data_length],",
        "COALESCE(cols.NUMERIC_PRECISION, c.precision) as [data_precision],",
        "COALESCE(cols.NUMERIC_SCALE, c.scale) as [data_scale],",
        "CASE WHEN cols.IS_NULLABLE = 'NO' THEN 1 ELSE 0 END as [not_null],",
        "CASE WHEN EXISTS (SELECT 1 FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE k",
        "JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS tc",
        "ON tc.CONSTRAINT_CATALOG = k.CONSTRAINT_CATALOG",
        "AND tc.CONSTRAINT_SCHEMA = k.CONSTRAINT_SCHEMA AND tc.CONSTRAINT_NAME = k.CONSTRAINT_NAME",
        "AND tc.TABLE_SCHEMA = k.TABLE_SCHEMA AND tc.TABLE_NAME = k.TABLE_NAME",
        "WHERE tc.CONSTRAINT_TYPE = 'PRIMARY KEY' AND k.TABLE_CATALOG = cols.TABLE_CATALOG",
        "AND k.TABLE_SCHEMA = cols.TABLE_SCHEMA AND k.TABLE_NAME = cols.TABLE_NAME",
        "AND k.COLUMN_NAME = cols.COLUMN_NAME) THEN 1 ELSE 0 END as [primary_key],",
        "col_description(t.object_id, c.column_id) as [comment]",
        "FROM INFORMATION_SCHEMA.COLUMNS cols",
        "JOIN sys.tables t ON t.name = cols.TABLE_NAME AND t.schema_id = SCHEMA_ID(cols.TABLE_SCHEMA)",
        "JOIN sys.columns c ON c.object_id = t.object_id AND c.name = cols.COLUMN_NAME",
        "JOIN pg_attribute a ON a.attrelid = t.object_id AND a.attnum = c.column_id",
        "WHERE cols.TABLE_SCHEMA = #{schema} AND cols.TABLE_NAME LIKE #{table}",
        "ORDER BY cols.TABLE_NAME, cols.ORDINAL_POSITION");

    public KingbaseSqlServerTableMetadataParser(RDBSchemaMetadata schema) {
        super(schema);
    }

    @Override
    protected String getTableMetaSql(String name) {
        return TABLE_META_SQL;
    }

    @Override
    protected String getTableCommentSql(String name) {
        return "SELECT cast(ep.value as nvarchar(500)) as [comment], t.name as [table_name]"
            + " FROM sys.tables t JOIN sys.extended_properties ep ON ep.major_id = t.object_id"
            + " AND ep.minor_id = 0 AND ep.class = 1 AND ep.name = 'MS_Description'"
            + " WHERE t.schema_id = SCHEMA_ID(#{schema}) AND t.name LIKE #{table}";
    }

    @Override
    protected String getAllTableSql() {
        return "SELECT name as [name] FROM sys.tables WHERE schema_id = SCHEMA_ID(#{schema})";
    }

    @Override
    protected String getTableExistsSql() {
        return "SELECT count(1) as [total] FROM sys.tables WHERE schema_id = SCHEMA_ID(#{schema}) AND name = #{table}";
    }

    @Override
    public Mono<RDBTableMetadata> parseByNameReactive(String name) {
        // Keep the upstream parser unchanged. Its singleMap wrapper stops iteration before
        // the R2DBC executor emits the comment row, so use record wrappers in this adapter.
        return tableExistsReactive(name)
            .filter(Boolean::booleanValue)
            .flatMap(exists -> {
                RDBTableMetadata table = createTable(name);
                table.setName(name);
                table.setAlias(name);
                Map<String, Object> parameters = Map.of("table", name, "schema", schema.getName());
                Mono<Void> columns = getReactiveSqlExecutor()
                    .select(template(getTableMetaSql(name), parameters), new RecordResultWrapper())
                    .doOnNext(record -> {
                        record.getString("table_name").ifPresent(table::setRealName);
                        RDBColumnMetadata column = table.newColumn();
                        applyColumnInfo(column, record);
                        table.addColumn(column);
                    })
                    .then();
                Mono<Void> comments = getReactiveSqlExecutor()
                    .select(template(getTableCommentSql(name), parameters), new RecordResultWrapper())
                    .doOnNext(record -> record.getString("comment").ifPresent(table::setComment))
                    .then();
                Flux<RDBIndexMetadata> indexes = schema.findFeature(IndexMetadataParser.ID)
                    .map(parser -> parser.parseTableIndexReactive(name))
                    .orElseGet(Flux::empty)
                    .doOnNext(table::addIndex);
                return columns.then(comments).thenMany(indexes).then(Mono.just(table));
            });
    }

    @Override
    protected void applyColumnInfo(RDBColumnMetadata column, Record record) {
        for (String key : new String[]{"data_length", "data_precision", "data_scale"}) {
            record.get(key).filter(String.class::isInstance)
                .ifPresent(value -> record.putValue(key, Integer.valueOf((String) value)));
        }
        super.applyColumnInfo(column, record);
    }
}
