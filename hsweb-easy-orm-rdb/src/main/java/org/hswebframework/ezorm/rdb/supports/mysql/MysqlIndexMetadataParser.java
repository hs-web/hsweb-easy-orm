package org.hswebframework.ezorm.rdb.supports.mysql;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.parser.IndexMetadataParser;

import java.util.*;

@Getter
@AllArgsConstructor
@Slf4j
public class MysqlIndexMetadataParser implements IndexMetadataParser {

    @Getter
    private RDBSchemaMetadata schema;

    @Override
    public List<RDBIndexMetadata> parseTableIndex(String tableName) {
        return schema.findFeature(SyncSqlExecutor.ID)
                .map(sqlExecutor -> sqlExecutor.select(SqlRequests.of("show index from ".concat(schema.getName()).concat(".").concat(tableName)),
                        new MysqlIndexWrapper()))
                .orElseGet(() -> {
                    log.warn("unsupported SyncSqlExecutor");
                    return Collections.emptyList();
                });
    }

    @Override
    public <T extends ObjectMetadata> Optional<T> parseByName(String name) {
        return Optional.empty();
    }

    @Override
    public  List<RDBIndexMetadata> parseAll() {
        return Collections.emptyList();
    }

    class MysqlIndexWrapper implements ResultWrapper<Map<String, String>, List<RDBIndexMetadata>> {
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
            String name = result.get("key_name");

            RDBIndexMetadata index = groupByName.computeIfAbsent(name, __ -> new RDBIndexMetadata());
            index.setName(result.get("key_name"));
            index.setUnique("0".equals(result.get("non_unique")));
            index.setTableName(result.get("table"));
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
