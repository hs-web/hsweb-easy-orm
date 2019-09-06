package org.hswebframework.ezorm.rdb.metadata;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.delete.DefaultDeleteSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.BatchInsertSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.update.DefaultUpdateSqlBuilder;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

@Getter
@Setter
public class RDBTableMetadata extends AbstractTableOrViewMetadata implements Cloneable {

    private String comment;

    private List<RDBIndexMetadata> indexes = new ArrayList<>();

    private List<RDBKeyMetadata> keys = new ArrayList<>();

    public RDBTableMetadata(String name) {
        this();
        setName(name);
    }

    public RDBTableMetadata() {
        super();
        addFeature(BatchInsertSqlBuilder.of(this));
        addFeature(DefaultUpdateSqlBuilder.of(this));

        addFeature(DefaultDeleteSqlBuilder.of(this));
    }

    @Override
    public ObjectType getObjectType() {
        return RDBObjectType.table;
    }

    @Override
    @SneakyThrows
    public RDBTableMetadata clone() {
        RDBTableMetadata clone = (RDBTableMetadata) super.clone();
        clone.setAllColumns(new ConcurrentHashMap<>());
        getColumns()
                .stream()
                .map(RDBColumnMetadata::clone)
                .forEach(clone::addColumn);

        clone.setFeatures(new HashMap<>(getFeatures()));
        clone.setIndexes(new ArrayList<>(getIndexes()));
        clone.setKeys(new ArrayList<>(getKeys()));
        clone.setForeignKey(new ArrayList<>(getForeignKey()));
        return clone;
    }
}
