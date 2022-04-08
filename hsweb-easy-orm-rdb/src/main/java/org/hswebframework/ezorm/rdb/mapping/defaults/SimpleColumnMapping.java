package org.hswebframework.ezorm.rdb.mapping.defaults;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;

import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;

@AllArgsConstructor(staticName = "of")
public class SimpleColumnMapping implements EntityColumnMapping {
    @Getter
    private Class<?> entityType;


    private Supplier<? extends TableOrViewMetadata> metadata;


    public static SimpleColumnMapping of(Class<?> type,TableOrViewMetadata metadata){
        return of(type,()->metadata);
    }


    @Override
    public Optional<RDBColumnMetadata> getColumnByProperty(String property) {
        return  metadata.get().findColumn(property);
    }

    @Override
    public Optional<String> getPropertyByColumnName(String columnName) {
        return metadata.get().findColumn(columnName)
                .map(RDBColumnMetadata::getAlias);
    }

    @Override
    public Optional<RDBColumnMetadata> getColumnByName(String columnName) {
        return metadata.get().findColumn(columnName);
    }

    @Override
    public Map<String, String> getColumnPropertyMapping() {
        return metadata.get().getColumns()
                .stream()
                .collect(Collectors.toMap(RDBColumnMetadata::getName, RDBColumnMetadata::getAlias));
    }

    @Override
    public TableOrViewMetadata getTable() {
        return metadata.get();
    }

    @Override
    public String getId() {
        return "SimpleColumnMapping";
    }

    @Override
    public String getName() {
        return "SimpleColumnMapping";
    }
}
