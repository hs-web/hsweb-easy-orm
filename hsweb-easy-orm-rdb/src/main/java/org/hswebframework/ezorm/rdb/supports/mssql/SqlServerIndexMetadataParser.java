package org.hswebframework.ezorm.rdb.supports.mssql;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.rdb.codec.JdbcResultDecoder;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.parser.IndexMetadataParser;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.function.Tuple2;
import reactor.util.function.Tuples;

import java.util.*;

@AllArgsConstructor
public class SqlServerIndexMetadataParser implements IndexMetadataParser {
    @Getter
    private final RDBSchemaMetadata schema;

    static String selectIndexSql = String.join(
            " ",
            "SELECT",
            "    t.name AS [table_name],",
            "    i.name AS [index_name],",
            "    c.name AS [column_name],",
            "    i.is_primary_key AS [primary_key],",
            "    i.is_unique AS [is_unique],",
            "    ic.key_ordinal AS [key_ordinal],",
            "    ic.is_descending_key AS [is_desc]",
            "FROM",
            "    sys.indexes i",
            "    JOIN sys.tables t ON i.object_id = t.object_id",
            "    JOIN sys.index_columns ic ON i.object_id = ic.object_id AND i.index_id = ic.index_id",
            "    JOIN sys.columns c ON ic.object_id = c.object_id AND ic.column_id = c.column_id",
            "WHERE",
            "    t.schema_id = SCHEMA_ID(?) and t.name like ? and i.name like ?",
            "ORDER BY",
            "    t.name, i.name, ic.key_ordinal;");


    @Override
    public List<RDBIndexMetadata> parseTableIndex(String tableName) {
        return schema.findFeatureNow(SyncSqlExecutor.ID)
                     .select(SqlRequests.of(selectIndexSql, schema.getName(), tableName, "%%"),
                             ResultWrappers.lowerCase(new IndexWrapper()));
    }

    @Override
    public Optional<RDBIndexMetadata> parseByName(String name) {
        return schema.findFeatureNow(SyncSqlExecutor.ID)
                     .select(SqlRequests.of(selectIndexSql, schema.getName(), "%%", name),
                             new IndexWrapper())
                     .stream()
                     .findAny()
                ;
    }

    @Override
    public List<RDBIndexMetadata> parseAll() {
        return schema.findFeatureNow(SyncSqlExecutor.ID)
                     .select(SqlRequests.of(selectIndexSql, schema.getName(), "%%", "%%"),
                             ResultWrappers.lowerCase(new IndexWrapper()));
    }

    @Override
    public Flux<RDBIndexMetadata> parseAllReactive() {
        return doSelectReactive(SqlRequests.of(selectIndexSql, schema.getName(), "%%", "%%"));
    }

    protected Flux<RDBIndexMetadata> doSelectReactive(SqlRequest sqlRequest) {
        IndexWrapper wrapper = new IndexWrapper();

        return schema.findFeatureNow(ReactiveSqlExecutor.ID)
                     .select(sqlRequest, ResultWrappers.lowerCase(wrapper))
                     .thenMany(Flux.defer(() -> Flux.fromIterable(wrapper.getResult())))
                ;
    }

    @Override
    public Mono<RDBIndexMetadata> parseByNameReactive(String name) {
        return doSelectReactive(SqlRequests.of(selectIndexSql, schema.getName(), "%%", name))
                .singleOrEmpty();
    }

    @Override
    public Flux<RDBIndexMetadata> parseTableIndexReactive(String tableName) {
        return doSelectReactive(SqlRequests.of(selectIndexSql, schema.getName(), tableName, "%%"));
    }

    static class IndexWrapper implements ResultWrapper<Map<String, String>, List<RDBIndexMetadata>> {
        Map<Tuple2<String, String>, RDBIndexMetadata> groupByName = new HashMap<>();

        @Override
        public Map<String, String> newRowInstance() {
            return new HashMap<>();
        }

        @Override
        public void wrapColumn(ColumnWrapperContext<Map<String, String>> context) {
            if (context.getResult() != null) {
                context.getRowInstance().put(
                        context.getColumnLabel().toLowerCase(),
                        String.valueOf(JdbcResultDecoder.INSTANCE.decode(context.getResult())));
            }
        }

        @Override
        public boolean completedWrapRow(Map<String, String> result) {
            String name = result.get("index_name");
            String tableName = result.get("table_name");

            RDBIndexMetadata index = groupByName.computeIfAbsent(Tuples.of(tableName, name), __ -> new RDBIndexMetadata());
            index.setName(result.get("index_name"));
            index.setUnique("true".equals(result.get("is_unique")));
            index.setTableName(tableName);
            index.setPrimaryKey("true".equals(result.get("primary_key")));
            RDBIndexMetadata.IndexColumn column = new RDBIndexMetadata.IndexColumn();
            column.setColumn(result.get("column_name"));
            column.setSortIndex(Integer.parseInt(result.get("key_ordinal")));
            column.setSort("true".equals(result.get("is_desc")) ? RDBIndexMetadata.IndexSort.asc : RDBIndexMetadata.IndexSort.desc);
            index.getColumns().add(column);
            return true;
        }

        @Override
        public List<RDBIndexMetadata> getResult() {
            return new LinkedList<>(groupByName.values());
        }
    }
}
