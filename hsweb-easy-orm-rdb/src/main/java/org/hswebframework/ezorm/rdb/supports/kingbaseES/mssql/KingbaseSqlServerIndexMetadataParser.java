package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerIndexMetadataParser;
import reactor.core.publisher.Flux;
import reactor.util.function.Tuple2;
import reactor.util.function.Tuples;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;

/** Uses native SQL Server views; bracket quoting prevents PostgreSQL array subscripts in this mode. */
public class KingbaseSqlServerIndexMetadataParser extends SqlServerIndexMetadataParser {

    private static final String INDEX_SQL = String.join(" ",
        "SELECT t.name AS [table_name], i.name AS [index_name], c.name AS [column_name],",
        "i.is_primary_key AS [primary_key], i.is_unique AS [is_unique],",
        "ic.key_ordinal AS [key_ordinal], ic.is_descending_key AS [is_desc]",
        "FROM sys.indexes i JOIN sys.tables t ON i.object_id = t.object_id",
        "JOIN sys.index_columns ic ON i.object_id = ic.object_id AND i.index_id = ic.index_id",
        "JOIN sys.columns c ON ic.object_id = c.object_id AND ic.column_id = c.column_id",
        "WHERE t.schema_id = SCHEMA_ID(?) AND t.name LIKE ? AND i.name LIKE ? AND ic.key_ordinal > 0",
        "ORDER BY t.name, i.name, ic.key_ordinal");

    public KingbaseSqlServerIndexMetadataParser(RDBSchemaMetadata schema) {
        super(schema);
    }

    @Override
    public List<RDBIndexMetadata> parseTableIndex(String tableName) {
        return select(tableName, "%%");
    }

    @Override
    public Optional<RDBIndexMetadata> parseByName(String name) {
        return select("%%", name).stream().findFirst();
    }

    @Override
    public List<RDBIndexMetadata> parseAll() {
        return select("%%", "%%");
    }

    private List<RDBIndexMetadata> select(String table, String index) {
        return getSchema().findFeatureNow(SyncSqlExecutor.ID)
            .select(SqlRequests.of(INDEX_SQL, getSchema().getName(), table, index), new IndexWrapper());
    }

    @Override
    protected Flux<RDBIndexMetadata> doSelectReactive(SqlRequest request) {
        return Flux.defer(() -> {
            IndexWrapper wrapper = new IndexWrapper();
            return getSchema().findFeatureNow(ReactiveSqlExecutor.ID)
                .select(SqlRequests.of(INDEX_SQL, request.getParameters()), wrapper)
                .thenMany(Flux.defer(() -> Flux.fromIterable(wrapper.getResult())));
        });
    }

    private static class IndexWrapper implements ResultWrapper<Map<String, Object>, List<RDBIndexMetadata>> {
        private final Map<Tuple2<String, String>, RDBIndexMetadata> indexes = new HashMap<>();

        @Override
        public Map<String, Object> newRowInstance() {
            return new HashMap<>();
        }

        @Override
        public void wrapColumn(ColumnWrapperContext<Map<String, Object>> context) {
            context.getRowInstance().put(context.getColumnLabel().toLowerCase(Locale.ROOT), context.getResult());
        }

        @Override
        public boolean completedWrapRow(Map<String, Object> row) {
            String table = String.valueOf(row.get("table_name"));
            String name = String.valueOf(row.get("index_name"));
            RDBIndexMetadata index = indexes.computeIfAbsent(Tuples.of(table, name), key -> new RDBIndexMetadata(name));
            index.setTableName(table);
            index.setPrimaryKey(flag(row.get("primary_key")));
            index.setUnique(flag(row.get("is_unique")));
            RDBIndexMetadata.IndexColumn column = new RDBIndexMetadata.IndexColumn();
            column.setColumn(String.valueOf(row.get("column_name")));
            column.setSortIndex(Integer.parseInt(String.valueOf(row.get("key_ordinal"))));
            column.setSort(flag(row.get("is_desc")) ? RDBIndexMetadata.IndexSort.desc : RDBIndexMetadata.IndexSort.asc);
            index.getColumns().add(column);
            return true;
        }

        @Override
        public List<RDBIndexMetadata> getResult() {
            return new ArrayList<>(indexes.values());
        }

        private static boolean flag(Object value) {
            return Boolean.TRUE.equals(value) || "1".equals(String.valueOf(value))
                || "t".equalsIgnoreCase(String.valueOf(value)) || "true".equalsIgnoreCase(String.valueOf(value));
        }
    }
}
