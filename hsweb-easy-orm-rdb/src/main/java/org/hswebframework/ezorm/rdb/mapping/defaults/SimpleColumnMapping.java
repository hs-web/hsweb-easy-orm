package org.hswebframework.ezorm.rdb.mapping.defaults;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;

import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@AllArgsConstructor(staticName = "of")
public class SimpleColumnMapping implements EntityColumnMapping {

    private TableOrViewMetadata metadata;

    @Override
    public Optional<RDBColumnMetadata> getColumnByProperty(String property) {
        return  metadata.findColumn(property);
    }

    @Override
    public Optional<String> getPropertyByColumnName(String columnName) {
        return metadata.findColumn(columnName)
                .map(RDBColumnMetadata::getAlias);
    }

    @Override
    public Optional<RDBColumnMetadata> getColumnByName(String columnName) {
        return metadata.findColumn(columnName);
    }

    @Override
    public Map<String, String> getColumnPropertyMapping() {
        return metadata.getColumns()
                .stream()
                .collect(Collectors.toMap(RDBColumnMetadata::getName, RDBColumnMetadata::getAlias));
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
