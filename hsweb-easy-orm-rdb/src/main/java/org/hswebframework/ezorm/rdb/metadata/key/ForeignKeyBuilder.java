package org.hswebframework.ezorm.rdb.metadata.key;

import lombok.*;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.operator.dml.JoinType;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ForeignKeyBuilder {

    private String name;

    private String alias;

    private boolean toMany;

    private boolean autoJoin;

    private String source;

    private String target;

    private AssociationType associationType;

    private List<Term> terms = new ArrayList<>();

    private JoinType joinType;

    private List<ForeignKeyBuilder> middleForeignKey;

    private List<ForeignKeyColumnBuilder> columns;

    public List<ForeignKeyColumnBuilder> getColumns() {
        return columns == null ? (columns = new ArrayList<>()) : columns;
    }

    public List<ForeignKeyBuilder> getMiddleForeignKey() {
        return middleForeignKey == null ? (middleForeignKey = new ArrayList<>()) : middleForeignKey;
    }

    public ForeignKeyBuilder addColumn(ForeignKeyColumnBuilder builder) {

        getColumns().add(builder);
        return this;
    }

    public ForeignKeyBuilder addMiddle(ForeignKeyBuilder builder) {
        getMiddleForeignKey().add(builder);
        return this;
    }


    public ForeignKeyBuilder addColumn(String sourceColumn, String targetColumn) {
        getColumns().add(ForeignKeyColumnBuilder.of(sourceColumn, targetColumn));
        return this;
    }


    @Getter
    @AllArgsConstructor(staticName = "of")
    public static class ForeignKeyColumnBuilder {
        private String sourceColumn;

        private String targetColumn;
    }

}
