package org.hswebframework.ezorm.rdb.supports.h2;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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

import java.util.*;

@AllArgsConstructor
@Slf4j
@SuppressWarnings("all")
public class H2IndexMetadataParser implements IndexMetadataParser {

    private RDBSchemaMetadata schema;

    private static final String sql = "select " +
            "index_name," +
            "table_name," +
            "column_name," +
            "primary_key," +
            "asc_or_desc," +
            "ordinal_position," +
            "non_unique " +
            "from information_schema.indexes " +
            "where table_name=? and table_schema=?";

    private static final String byName = "select " +
            "index_name," +
            "table_name," +
            "column_name," +
            "primary_key," +
            "asc_or_desc," +
            "ordinal_position," +
            "non_unique " +
            "from information_schema.indexes " +
            "where index_name=? and table_schema=?";

    private static final String all = "select " +
            "index_name," +
            "table_name," +
            "column_name," +
            "primary_key," +
            "asc_or_desc," +
            "ordinal_position," +
            "non_unique " +
            "from information_schema.indexes " +
            "where table_schema=?";

    @Override
    public List<RDBIndexMetadata> parseTableIndex(String tableName) {
        return doSelect(SqlRequests.of(sql, tableName.toUpperCase(), schema.getName().toUpperCase()));
    }

    @Override
    public Optional<RDBIndexMetadata> parseByName(String name) {
        return doSelect(SqlRequests.of(byName, name.toUpperCase(), schema.getName().toUpperCase()))
                .stream()
                .findFirst();
    }

    @Override
    public List<RDBIndexMetadata> parseAll() {
        return doSelect(SqlRequests.of(all, schema.getName().toUpperCase()));
    }

    @Override
    public Flux<RDBIndexMetadata> parseAllReactive() {
        return doSelectReactive(SqlRequests.of(all, schema.getName().toUpperCase()));
    }

    @Override
    public Mono<RDBIndexMetadata> parseByNameReactive(String name) {
        return doSelectReactive(SqlRequests.of(byName, name.toUpperCase(), schema.getName().toUpperCase()))
                .singleOrEmpty();
    }

    @Override
    public Flux<RDBIndexMetadata> parseTableIndexReactive(String tableName) {
        return doSelectReactive(SqlRequests.of(sql, tableName.toUpperCase(), schema.getName().toUpperCase()));
    }

    protected Flux<RDBIndexMetadata> doSelectReactive(SqlRequest sqlRequest) {
        H2IndexMetadataWrapper wrapper = new H2IndexMetadataWrapper();

        return schema.<ReactiveSqlExecutor>findFeatureNow(ReactiveSqlExecutor.ID)
                .select(sqlRequest, ResultWrappers.map())
                .doOnNext(wrapper::completedWrapRow)
                .thenMany(Flux.defer(() -> Flux.fromIterable(wrapper.getResult())))
                ;
    }

    protected List<RDBIndexMetadata> doSelect(SqlRequest sqlRequest) {
        return schema.<SyncSqlExecutor>findFeatureNow(SyncSqlExecutor.ID)
                .select(sqlRequest, new H2IndexMetadataWrapper());
    }

    class H2IndexMetadataWrapper implements ResultWrapper<Map<String, Object>, List<RDBIndexMetadata>> {
        Map<String, RDBIndexMetadata> group = new LinkedHashMap<>();

        @Override
        public Map<String, Object> newRowInstance() {
            return new HashMap<>();
        }

        @Override
        public void wrapColumn(ColumnWrapperContext<Map<String, Object>> context) {
            context.getRowInstance().put(context.getColumnLabel().toLowerCase(), context.getResult());
        }

        @Override
        public boolean completedWrapRow(Map<String, Object> result) {
            String name = (String) result.get("index_name");
            RDBIndexMetadata index = group.computeIfAbsent(name, __ -> new RDBIndexMetadata());
            index.setName(name.toLowerCase());
            index.setTableName(((String) result.get("table_name")).toLowerCase());
            index.setPrimaryKey(Boolean.TRUE.equals(result.get("primary_key")));
            index.setUnique(Boolean.FALSE.equals(result.get("non_unique")));
            RDBIndexMetadata.IndexColumn indexColumn = new RDBIndexMetadata.IndexColumn();
            indexColumn.setColumn(((String) result.get("column_name")).toLowerCase());
            indexColumn.setSort("A".equals(result.get("asc_or_desc")) ?
                    RDBIndexMetadata.IndexSort.asc : RDBIndexMetadata.IndexSort.desc);
            indexColumn.setSortIndex(((Number) result.get("ordinal_position")).intValue());
            index.getColumns().add(indexColumn);
            return true;
        }


        @Override
        public List<RDBIndexMetadata> getResult() {
            return new ArrayList<>(group.values());
        }
    }
}
