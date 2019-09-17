package org.hswebframework.ezorm.rdb.metadata;

import lombok.AllArgsConstructor;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.operator.dml.JoinType;

import java.util.List;

@AllArgsConstructor(staticName = "of")
public class LazyForeignKeyMetadata implements ForeignKeyMetadata {

    private ForeignKeyBuilder builder;

    private TableOrViewMetadata mainTable;

    @Override
    public boolean isLogical() {
        return true;
    }

    @Override
    public boolean isToMany() {
        return builder.isToMany();
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
    public RDBColumnMetadata getSourceColumn() {
        return getSource().getColumn(builder.getSourceColumn())
                .orElseThrow(() -> new IllegalArgumentException("column [" + builder.getSourceColumn() + "] doesn't exist on table [" + mainTable.getName() + "]"));
    }

    @Override
    public RDBColumnMetadata getTargetColumn() {
        return getTarget()
                .getColumn(builder.getSourceColumn())
                .orElseThrow(() -> new IllegalArgumentException("column [" + builder.getSourceColumn() + "] doesn't exist on table [" + mainTable.getName() + "]"));

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
    public List<ForeignKeyMetadata> getPreForeignKey() {
        return builder.getPreForeignKey();
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
        return (LazyForeignKeyMetadata)super.clone();
    }
}
