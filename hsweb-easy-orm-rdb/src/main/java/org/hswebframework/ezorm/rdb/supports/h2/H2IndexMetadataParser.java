package org.hswebframework.ezorm.rdb.supports.h2;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapperContext;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.parser.IndexMetadataParser;

import java.util.*;
import java.util.stream.Collectors;

@AllArgsConstructor(staticName = "of")
@Slf4j
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

    @Override
    public List<RDBIndexMetadata> parseTableIndex(String tableName) {
        return schema.<SyncSqlExecutor>findFeature(SyncSqlExecutor.id)
                .map(sqlExecutor -> sqlExecutor.select(SqlRequests.of(sql, tableName.toUpperCase(), schema.getName().toUpperCase()),
                        new H2IndexMetadataWrapper()))
                .orElseGet(() -> {
                    log.warn("unsupported SyncSqlExecutor");
                    return Collections.emptyList();
                });


    }

    class H2IndexMetadataWrapper implements ResultWrapper<Map<String, Object>, List<RDBIndexMetadata>> {
        Map<String, RDBIndexMetadata> group = new LinkedHashMap<>();

        @Override
        public Map<String, Object> newRowInstance() {
            return new HashMap<>();
        }

        @Override
        public void wrapColumn(ColumnWrapperContext<Map<String, Object>> context) {
            context.getInstance().put(context.getColumnLabel().toLowerCase(), context.getResult());
        }

        @Override
        public boolean completedWrapRow( Map<String, Object> result) {
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
