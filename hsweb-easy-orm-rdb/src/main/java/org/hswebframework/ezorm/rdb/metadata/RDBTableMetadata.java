package org.hswebframework.ezorm.rdb.metadata;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.delete.DefaultDeleteSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.BatchInsertSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.update.DefaultUpdateSqlBuilder;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
public class RDBTableMetadata extends AbstractTableOrViewMetadata {

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
}
