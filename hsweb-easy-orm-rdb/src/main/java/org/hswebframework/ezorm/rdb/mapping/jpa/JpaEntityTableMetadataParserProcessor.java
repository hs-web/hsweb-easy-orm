package org.hswebframework.ezorm.rdb.mapping.jpa;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.beanutils.BeanUtilsBean;
import org.hswebframework.ezorm.rdb.mapping.EntityPropertyDescriptor;
import org.hswebframework.ezorm.rdb.mapping.annotation.Comment;
import org.hswebframework.ezorm.rdb.mapping.parser.DataTypeResolver;
import org.hswebframework.ezorm.rdb.mapping.DefaultEntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.parser.ValueCodecResolver;
import org.hswebframework.ezorm.rdb.metadata.*;
import org.hswebframework.ezorm.rdb.metadata.dialect.DataTypeBuilder;
import org.hswebframework.ezorm.rdb.metadata.key.AssociationType;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyBuilder;
import org.hswebframework.ezorm.rdb.utils.AnnotationUtils;
import org.hswebframework.ezorm.rdb.utils.PropertiesUtils;
import org.hswebframework.utils.ClassUtils;
import org.reactivestreams.Publisher;

import javax.persistence.*;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.*;
import java.util.stream.Stream;

import static java.util.Optional.*;
import static org.hswebframework.ezorm.rdb.utils.AnnotationUtils.getAnnotation;
import static org.hswebframework.ezorm.rdb.utils.AnnotationUtils.getAnnotations;

@Slf4j
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
        List<Runnable> afterRun = new ArrayList<>();

        for (PropertyDescriptor descriptor : descriptors) {
            Set<Annotation> annotations = getAnnotations(entityType, descriptor);

            getAnnotation(annotations, Column.class)
                    .ifPresent(column -> handleColumnAnnotation(descriptor, annotations, ColumnInfo.of(column)));

            getAnnotation(annotations, JoinColumns.class)
                    .ifPresent(column -> afterRun.add(() -> handleJoinColumnAnnotation(descriptor, annotations, column.value())));

            getAnnotation(annotations, JoinColumn.class)
                    .ifPresent(column -> afterRun.add(() -> handleJoinColumnAnnotation(descriptor, annotations, column)));

        }
        afterRun.forEach(Runnable::run);
    }

    private <T extends Annotation> Optional<T> getAnnotation(Set<Annotation> annotations, Class<T> type) {
        return annotations.stream()
                .filter(type::isInstance)
                .map(type::cast)
                .findFirst();
    }

    @SneakyThrows
    private void handleJoinColumnAnnotation(PropertyDescriptor descriptor, Set<Annotation> annotations, JoinTable column) {

    }

    @SneakyThrows
    private void handleJoinColumnAnnotation(PropertyDescriptor descriptor, Set<Annotation> annotations, JoinColumn... column) {

        Field field = PropertiesUtils.getPropertyField(entityType, descriptor.getName())
                .orElseThrow(() -> new NoSuchFieldException("no such field " + descriptor.getName() + " in " + entityType));
        Table join;
        ForeignKeyBuilder builder = ForeignKeyBuilder.builder()
                .source(tableMetadata.getFullName())
                .name(descriptor.getName())
                .alias(descriptor.getName())
                .build();

        Type fieldGenericType = field.getGenericType();
        if (fieldGenericType instanceof ParameterizedType) {
            Type[] types = ((ParameterizedType) fieldGenericType).getActualTypeArguments();
            join = Stream.of(types)
                    .map(Class.class::cast)
                    .map(t -> AnnotationUtils.getAnnotation(t, Table.class))
                    .filter(Objects::nonNull)
                    .findFirst()
                    .orElse(null);
        } else {
            builder.setAutoJoin(true);
            join = AnnotationUtils.getAnnotation(field.getType(), Table.class);
        }
        String joinTableName;
        if (join != null) {
            joinTableName = join.schema().isEmpty() ? join.name() : join.schema().concat(".").concat(join.name());
        } else {
            log.warn("can not resolve join table for :{}", field);
            return;
        }
        builder.setTarget(joinTableName);

        getAnnotation(annotations, OneToOne.class)
                .ifPresent(oneToOne -> builder.setAssociationType(AssociationType.oneToOne));
        getAnnotation(annotations, OneToMany.class)
                .ifPresent(oneToOne -> builder.setAssociationType(AssociationType.oneToMay));
        getAnnotation(annotations, ManyToMany.class)
                .ifPresent(oneToOne -> builder.setAssociationType(AssociationType.manyToMay));
        getAnnotation(annotations, ManyToOne.class)
                .ifPresent(oneToOne -> builder.setAssociationType(AssociationType.manyToOne));


        for (JoinColumn joinColumn : column) {

            String columnName = joinColumn.name();

            builder.addColumn(columnName, joinColumn.referencedColumnName());
        }
        tableMetadata.addForeignKey(builder);
    }

    @Getter
    @SuppressWarnings("all")
    private static class ColumnInfo {
        private String name = "";
        private String table = "";

        private boolean nullable;
        private boolean updatable;
        private boolean insertable;

        private int length;

        private int precision;
        private int scale;

        private String columnDefinition = "";

        public static ColumnInfo of(JoinColumn column) {
            ColumnInfo columnInfo = new ColumnInfo();
            columnInfo.insertable = column.insertable();
            columnInfo.updatable = column.updatable();
            columnInfo.nullable = column.nullable();
            columnInfo.name = column.name();
            columnInfo.table = column.table();
            return columnInfo;
        }

        public static ColumnInfo of(Column column) {
            ColumnInfo columnInfo = new ColumnInfo();
            columnInfo.insertable = column.insertable();
            columnInfo.updatable = column.updatable();
            columnInfo.nullable = column.nullable();
            columnInfo.name = column.name();
            columnInfo.table = column.table();
            columnInfo.length=column.length();
            columnInfo.scale=column.scale();
            columnInfo.precision=column.precision();

            return columnInfo;
        }
    }

    private void handleColumnAnnotation(PropertyDescriptor descriptor, Set<Annotation> annotations, ColumnInfo column) {
        //另外一个表
        if (!column.table.isEmpty() && !column.table.equals(tableMetadata.getName())) {
            mapping.addMapping(column.table.concat(".").concat(column.name), descriptor.getName());
            return;
        }
        String columnName;

        if (!column.name.isEmpty()) {
            columnName = column.name;
        } else {
            columnName = descriptor.getName();
        }
        Class javaType = descriptor.getPropertyType();

        if (javaType == Object.class) {
            javaType = descriptor.getReadMethod().getReturnType();
        }
        mapping.addMapping(columnName, descriptor.getName());
        RDBColumnMetadata metadata = tableMetadata.getColumn(columnName).orElseGet(tableMetadata::newColumn);
        metadata.setName(columnName);
        metadata.setAlias(descriptor.getName());
        metadata.setJavaType(javaType);
        metadata.setLength(column.length);
        metadata.setPrecision(column.precision);
        metadata.setScale(column.scale);
        metadata.setNotNull(!column.nullable);
        metadata.setUpdatable(column.updatable);
        if (!column.columnDefinition.isEmpty()) {
            metadata.setColumnDefinition(column.columnDefinition);
        }
        getAnnotation(annotations, Comment.class)
                .map(Comment::value)
                .ifPresent(metadata::setComment);

        getAnnotation(annotations, Id.class).ifPresent(id -> metadata.setPrimaryKey(true));

        EntityPropertyDescriptor propertyDescriptor = SimpleEntityPropertyDescriptor.of(entityType, descriptor.getName(), javaType, metadata, descriptor);

        metadata.addFeature(propertyDescriptor);

        ofNullable(dataTypeResolver)
                .map(resolver -> resolver.resolve(propertyDescriptor))
                .ifPresent(metadata::setType);

        if (metadata.getType() == null) {
            tableMetadata
                    .getDialect()
                    .convertSqlType(metadata.getJavaType())
                    .ifPresent(jdbcType -> metadata.setJdbcType(jdbcType, metadata.getJavaType()));
        }

        ofNullable(valueCodecResolver)
                .map(resolver -> resolver.resolve(propertyDescriptor)
                        .orElseGet(() -> metadata.findFeature(ValueCodecFactory.ID)
                                .flatMap(factory -> factory.createValueCodec(metadata))
                                .orElse(null)))
                .ifPresent(metadata::setValueCodec);
        ;

        tableMetadata.addColumn(metadata);
    }


}
