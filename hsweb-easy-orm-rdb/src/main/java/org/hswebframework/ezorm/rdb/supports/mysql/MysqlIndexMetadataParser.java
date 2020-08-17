package org.hswebframework.ezorm.rdb.supports.mysql;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
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
import org.hswebframework.ezorm.rdb.supports.h2.H2IndexMetadataParser;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.*;

@Getter
@AllArgsConstructor
@Slf4j
public class MysqlIndexMetadataParser implements IndexMetadataParser {

    @Getter
    private final RDBSchemaMetadata schema;

    static String selectIndexSql = String.join(" ",
            "SELECT",
            "*",
            "FROM",
            "INFORMATION_SCHEMA.STATISTICS",
            "WHERE",
            "TABLE_SCHEMA = ? and TABLE_NAME like ?");

    static String selectIndexSqlByName = String.join(" ",
            "SELECT",
            "*",
            "FROM",
            "INFORMATION_SCHEMA.STATISTICS",
            "WHERE",
            "TABLE_SCHEMA = ? and INDEX_NAME=?");

    @Override
    public List<RDBIndexMetadata> parseTableIndex(String tableName) {
        return schema.findFeatureNow(SyncSqlExecutor.ID)
                .select(SqlRequests.of(selectIndexSql, schema.getName(), tableName),
                        ResultWrappers.lowerCase(new MysqlIndexWrapper()));
    }

    @Override
    public Optional<RDBIndexMetadata> parseByName(String name) {
        return schema.findFeatureNow(SyncSqlExecutor.ID)
                .select(SqlRequests.of(selectIndexSqlByName, schema.getName(), name),
                        new MysqlIndexWrapper())
                .stream()
                .findAny()
                ;
    }

    @Override
    public List<RDBIndexMetadata> parseAll() {
        return schema.findFeatureNow(SyncSqlExecutor.ID)
                .select(SqlRequests.of(selectIndexSql, schema.getName(), "%%"),
                        ResultWrappers.lowerCase(new MysqlIndexWrapper()));
    }

    @Override
    public Flux<RDBIndexMetadata> parseAllReactive() {
        return doSelectReactive(SqlRequests.of(selectIndexSql, schema.getName(), "%%"));
    }

    protected Flux<RDBIndexMetadata> doSelectReactive(SqlRequest sqlRequest) {
        MysqlIndexWrapper wrapper = new MysqlIndexWrapper();

        return schema.findFeatureNow(ReactiveSqlExecutor.ID)
                .select(sqlRequest,ResultWrappers.lowerCase(wrapper))
                .thenMany(Flux.defer(() -> Flux.fromIterable(wrapper.getResult())))
                ;
    }

    @Override
    public Mono<RDBIndexMetadata> parseByNameReactive(String name) {
        return doSelectReactive(SqlRequests.of(selectIndexSqlByName, schema.getName(), name))
                .singleOrEmpty();
    }

    @Override
    public Flux<RDBIndexMetadata> parseTableIndexReactive(String tableName) {
        return doSelectReactive(SqlRequests.of(selectIndexSql, schema.getName(), tableName));
    }

    static class MysqlIndexWrapper implements ResultWrapper<Map<String, String>, List<RDBIndexMetadata>> {
        Map<String, RDBIndexMetadata> groupByName = new HashMap<>();

        @Override
        public Map<String, String> newRowInstance() {
            return new HashMap<>();
        }

        @Override
        public void wrapColumn(ColumnWrapperContext<Map<String, String>> context) {
            if (context.getResult() != null) {
                context.getRowInstance().put(context.getColumnLabel().toLowerCase(), String.valueOf(context.getResult()));
            }
        }

        @Override
        public boolean completedWrapRow(Map<String, String> result) {
            String name = result.get("index_name");

            RDBIndexMetadata index = groupByName.computeIfAbsent(name, __ -> new RDBIndexMetadata());
            index.setName(result.get("index_name"));
            index.setUnique("0".equals(result.get("non_unique")));
            index.setTableName(result.get("table_name"));
            index.setPrimaryKey("PRIMARY".equalsIgnoreCase(name));
            RDBIndexMetadata.IndexColumn column = new RDBIndexMetadata.IndexColumn();
            column.setColumn(result.get("column_name"));
            column.setSortIndex(Integer.parseInt(result.get("seq_in_index")));
            column.setSort(RDBIndexMetadata.IndexSort.asc);
            index.getColumns().add(column);
            return true;
        }

        @Override
        public List<RDBIndexMetadata> getResult() {
            return new LinkedList<>(groupByName.values());
        }
    }
}
