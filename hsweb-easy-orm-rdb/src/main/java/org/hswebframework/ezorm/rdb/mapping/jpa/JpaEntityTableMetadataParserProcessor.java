package org.hswebframework.ezorm.rdb.mapping.jpa;

import lombok.Setter;
import org.apache.commons.beanutils.BeanUtilsBean;
import org.hswebframework.ezorm.rdb.mapping.annotation.Comment;
import org.hswebframework.ezorm.rdb.mapping.parser.DataTypeResolver;
import org.hswebframework.ezorm.rdb.mapping.DefaultEntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.parser.ValueCodecResolver;
import org.hswebframework.ezorm.rdb.metadata.*;
import org.hswebframework.ezorm.rdb.utils.AnnotationUtils;
import org.hswebframework.utils.ClassUtils;

import javax.persistence.*;
import java.beans.PropertyDescriptor;
import java.util.Optional;

import static java.util.Optional.*;
import static org.hswebframework.ezorm.rdb.utils.AnnotationUtils.getAnnotation;

public class JpaEntityTableMetadataParserProcessor {

    private DefaultEntityColumnMapping mapping;

    private Class<?> entityType;

    private RDBTableMetadata tableMetadata;

    @Setter
    private DataTypeResolver dataTypeResolver;

    @Setter
    private ValueCodecResolver valueCodecResolver;

    public JpaEntityTableMetadataParserProcessor(RDBTableMetadata tableMetadata, Class<?> entityType) {
        this.tableMetadata = tableMetadata;
        this.entityType = entityType;
        this.mapping = new DefaultEntityColumnMapping(tableMetadata, entityType);
        tableMetadata.addFeature(this.mapping);
    }

    public void process() {
        PropertyDescriptor[] descriptors = BeanUtilsBean.getInstance()
                .getPropertyUtils()
                .getPropertyDescriptors(entityType);

        //List<Runnable> lastRun = new ArrayList<>();

        Table table = ClassUtils.getAnnotation(entityType, Table.class);
        int idx = 0;

        for (Index index : table.indexes()) {
            String name = index.name();
            if (name.isEmpty()) {
                name = tableMetadata.getName().concat("_idx_").concat(String.valueOf(idx++));
            }
            RDBIndexMetadata indexMetadata = new RDBIndexMetadata();
            indexMetadata.setUnique(index.unique());
            indexMetadata.setName(name);

            //id asc,
            String[] columnList = index.columnList().split("[,]");
            for (String str : columnList) {
                String[] columnAndSort = str.split("[ ]+");
                RDBIndexMetadata.IndexColumn column = new RDBIndexMetadata.IndexColumn();
                column.setColumn(columnAndSort[0].trim());
                if (columnAndSort.length > 1) {
                    column.setSort(columnAndSort[1].equalsIgnoreCase("desc") ? RDBIndexMetadata.IndexSort.desc : RDBIndexMetadata.IndexSort.asc);
                }
                indexMetadata.getColumns().add(column);
            }
            tableMetadata.addIndex(indexMetadata);
        }

        for (PropertyDescriptor descriptor : descriptors) {
            Column column = getAnnotation(entityType, descriptor, Column.class);
            if (column != null) {
                handleColumnAnnotation(descriptor, column);
            }
            JoinColumns joinColumns = getAnnotation(entityType, descriptor, JoinColumns.class);
            if (null != joinColumns) {
                for (JoinColumn joinColumn : joinColumns.value()) {
                    handleJoinColumnAnnotation(joinColumn);
                }
            }
            JoinColumn joinColumn = getAnnotation(entityType, descriptor, JoinColumn.class);
            if (null != joinColumn) {
                handleJoinColumnAnnotation(joinColumn);
            }
        }

        //  lastRun.forEach(Runnable::run);
    }

    private void handleJoinColumnAnnotation(JoinColumn column) {

    }

    private void handleColumnAnnotation(PropertyDescriptor descriptor, Column column) {
        //另外一个表
        if (!column.table().isEmpty() && !column.table().equals(tableMetadata.getName())) {
            mapping.addMapping(column.table().concat(".").concat(column.name()), descriptor.getName());
            return;
        }
        String columnName;

        if (!column.name().isEmpty()) {
            columnName = column.name();
        } else {
            columnName = descriptor.getName();
        }
        mapping.addMapping(columnName, descriptor.getName());
        RDBColumnMetadata metadata = tableMetadata.getColumn(columnName).orElseGet(tableMetadata::newColumn);
        metadata.setName(columnName);
        metadata.setAlias(descriptor.getName());
        metadata.setJavaType(descriptor.getPropertyType());
        metadata.setLength(column.length());
        metadata.setPrecision(column.precision());
        metadata.setScale(column.scale());
        metadata.setNotNull(!column.nullable());
        metadata.setUpdatable(column.updatable());
        if (!column.columnDefinition().isEmpty()) {
            metadata.setColumnDefinition(column.columnDefinition());
        }
        Optional.ofNullable(AnnotationUtils.getAnnotation(entityType,descriptor, Comment.class))
                .map(Comment::value)
                .ifPresent(tableMetadata::setComment);

        ofNullable(getAnnotation(entityType, descriptor, Id.class))
                .ifPresent(id -> metadata.setPrimaryKey(true));

        ofNullable(dataTypeResolver)
                .map(resolver -> resolver.resolve(entityType, descriptor))
                .ifPresent(metadata::setType);

        if (metadata.getType() == null) {
            tableMetadata
                    .getDialect()
                    .convertJdbcType(metadata.getJavaType())
                    .ifPresent(jdbcType -> metadata.setJdbcType(jdbcType, metadata.getJavaType()));
        }

        ofNullable(valueCodecResolver)
                .map(resolver -> resolver.resolve(entityType, descriptor)
                        .orElseGet(() -> metadata.findFeature(ValueCodecFactory.ID)
                                .map(factory -> factory.createValueCodec(metadata))
                                .orElse(null)))
                .ifPresent(metadata::setValueCodec);

        metadata.setDataType(tableMetadata.getDialect().buildColumnDataType(metadata));

        tableMetadata.addColumn(metadata);
    }


}
