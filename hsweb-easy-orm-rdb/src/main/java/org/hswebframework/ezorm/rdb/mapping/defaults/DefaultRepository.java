package org.hswebframework.ezorm.rdb.mapping.defaults;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.ObjectPropertyOperator;
import org.hswebframework.ezorm.core.GlobalConfig;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertResultOperator;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class DefaultRepository<E> {

    protected RDBTableMetadata table;

    protected DatabaseOperator operator;

    protected Class<E> entityType;

    protected ResultWrapper<E, ?> wrapper;

    protected String idColumn;

    protected EntityColumnMapping mapping;

    private String[] properties;

    @Getter
    @Setter
    private ObjectPropertyOperator propertyOperator = GlobalConfig.getPropertyOperator();

    public DefaultRepository(DatabaseOperator operator, RDBTableMetadata table, Class<E> type, ResultWrapper<E, ?> wrapper) {
        this.operator = operator;
        this.table = table;
        this.entityType = type;
        this.wrapper = wrapper;
        init();
    }

    protected void init() {
        this.idColumn = table.getColumns().stream()
                .filter(RDBColumnMetadata::isPrimaryKey)
                .findFirst()
                .map(RDBColumnMetadata::getName)
                .orElse(null);
        this.mapping = table.<EntityColumnMapping>findFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(entityType))
                .orElseThrow(() -> new UnsupportedOperationException("unsupported columnPropertyMapping feature"));

        this.properties = mapping.getColumnPropertyMapping()
                .entrySet()
                .stream()
                .filter(kv -> table.getColumn(kv.getKey()).isPresent())
                .map(Map.Entry::getValue)
                .toArray(String[]::new);

    }

    protected InsertResultOperator doInsert(E data) {
        InsertOperator insert = operator.dml().insert(table.getFullName());

        for (Map.Entry<String, String> entry : mapping.getColumnPropertyMapping().entrySet()) {
            String column = entry.getKey();
            String property = entry.getValue();
            propertyOperator.getProperty(data, property)
                    .ifPresent(val -> insert.value(column, val));

        }

        return insert.execute();

    }

    protected InsertResultOperator doInsert(Collection<E> batch) {
        InsertOperator insert = operator.dml().insert(table.getFullName());

        insert.columns(properties);

        for (E e : batch) {
            insert.values(Stream.of(properties)
                    .map(property -> propertyOperator
                            .getProperty(e, property)
                            .orElseGet(() -> mapping.getColumnByProperty(property)
                                    .map(column -> NullValue.of(column.getJavaType(), column.getJdbcType()))
                                    .orElse(null)))
                    .toArray());
        }
        return insert.execute();
    }

}
