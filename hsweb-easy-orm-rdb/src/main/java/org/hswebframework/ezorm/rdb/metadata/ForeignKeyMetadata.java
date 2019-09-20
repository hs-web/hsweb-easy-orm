package org.hswebframework.ezorm.rdb.metadata;

import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.operator.dml.JoinType;

import java.util.List;

/**
 * 外键
 * @see ForeignKeyBuilder
 */
public interface ForeignKeyMetadata extends ObjectMetadata {

    @Override
    default ObjectType getObjectType() {
        return RDBObjectType.foreignKey;
    }

    /**
     * @return 是否为逻辑外键
     */
    boolean isLogical();

    /**
     * @return 是否n对多
     */
    boolean isToMany();

    TableOrViewMetadata getSource();

    TableOrViewMetadata getTarget();

    RDBColumnMetadata getSourceColumn();

    RDBColumnMetadata getTargetColumn();

    boolean isAutoJoin();

    //自动关联表类型
    JoinType getJoinType();

    List<Term> getTerms();

    List<ForeignKeyMetadata> getPreForeignKey();
}
