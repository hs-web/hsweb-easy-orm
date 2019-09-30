package org.hswebframework.ezorm.rdb.metadata.key;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.JoinType;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;


public class LazyForeignKeyMetadata implements ForeignKeyMetadata {

    private ForeignKeyBuilder builder;

    private TableOrViewMetadata mainTable;

    private List<ForeignKeyMetadata> preKey;

    private List<ForeignKeyColumn> columns;

    @Override
    public AssociationType getType() {
        return Optional.ofNullable(builder)
                .map(ForeignKeyBuilder::getAssociationType)
                .orElse(AssociationType.oneToOne);
    }

    public static LazyForeignKeyMetadata of(ForeignKeyBuilder builder, TableOrViewMetadata mainTable) {
        LazyForeignKeyMetadata metadata = new LazyForeignKeyMetadata();
        metadata.builder = builder;
        metadata.mainTable = mainTable;
        return metadata;
    }

    @Override
    public boolean isLogical() {
        return true;
    }

    @Override
    public boolean isToMany() {
        return getType().isToMany();
    }

    @Override
    public TableOrViewMetadata getSource() {
        return mainTable;
    }

    @Override
    public TableOrViewMetadata getTarget() {
        return mainTable.getSchema()
                .getTableOrView(builder.getTarget())
                .orElseThrow(() -> new IllegalArgumentException("target [" + builder.getTarget() + "] doesn't exist"));
    }

    @Override
    public List<ForeignKeyColumn> getColumns() {
        if (columns == null) {
            columns = builder.getColumns()
                    .stream()
                    .map(columnBuilder -> new ForeignKeyColumn() {
                        private RDBColumnMetadata targetCache;
                        private RDBColumnMetadata sourceCache;

                        @Override
                        public RDBColumnMetadata getTargetColumn() {
                            if (targetCache != null) {
                                return targetCache;
                            }
                            if (columnBuilder.getTargetColumn() == null || columnBuilder.getTargetColumn().isEmpty()) {
                                return targetCache = getTarget().getColumns()
                                        .stream().filter(RDBColumnMetadata::isPrimaryKey)
                                        .findFirst().orElseThrow(() -> new IllegalArgumentException("primary key column doesn't exist "));
                            }
                            return targetCache = getTarget().findColumn(columnBuilder.getTargetColumn())
                                    .orElseThrow(() -> new IllegalArgumentException("column [" + columnBuilder.getTargetColumn() + "] doesn't exist"));
                        }

                        @Override
                        public RDBColumnMetadata getSourceColumn() {
                            if (sourceCache != null) {
                                return sourceCache;
                            }
                            if (columnBuilder.getSourceColumn() == null || columnBuilder.getSourceColumn().isEmpty()) {
                                return sourceCache = mainTable.getColumns()
                                        .stream().filter(RDBColumnMetadata::isPrimaryKey)
                                        .findFirst().orElseThrow(() -> new IllegalArgumentException("primary key column doesn't exist "));
                            }
                            return sourceCache = mainTable.findColumn(columnBuilder.getSourceColumn())
                                    .orElseThrow(() -> new IllegalArgumentException("column [" + columnBuilder.getSourceColumn() + "] doesn't exist"));
                        }
                    }).collect(Collectors.toList());
        }
        return columns;
    }

    @Override
    public boolean isAutoJoin() {
        return builder.isAutoJoin();
    }

    @Override
    public JoinType getJoinType() {
        return builder.getJoinType();
    }

    @Override
    public List<Term> getTerms() {
        return builder.getTerms();
    }

    @Override
    public Optional<ForeignKeyMetadata> getMiddleForeignKey(String name) {
        return getMiddleForeignKeys()
                .stream()
                .filter(key -> key.equalsNameOrAlias(name) || key.getSource().equalsNameOrAlias(name))
                .findFirst();
    }

    @Override
    public List<ForeignKeyMetadata> getMiddleForeignKeys() {
        if (builder.getMiddleForeignKey() == null) {
            return Collections.emptyList();
        }
        if (preKey == null) {
            preKey = builder
                    .getMiddleForeignKey()
                    .stream()
                    .<ForeignKeyMetadata>map(builder -> LazyForeignKeyMetadata.of(builder, mainTable
                            .getSchema()
                            .getTableOrView(builder.getSource())
                            .orElseThrow(() -> new IllegalArgumentException("table or view [" + builder.getSource() + "] doesn't exist"))))
                    .collect(Collectors.toList());
        }
        return preKey;
    }

    @Override
    public String getName() {
        return builder.getName();
    }

    @Override
    public String getAlias() {
        return builder.getAlias();
    }

    @Override
    @SneakyThrows
    public LazyForeignKeyMetadata clone() {
        return (LazyForeignKeyMetadata) super.clone();
    }
}
