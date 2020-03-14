package org.hswebframework.ezorm.rdb.operator.dml.upsert;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.insert.InsertSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.update.UpdateSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateColumn;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperatorParameter;
import org.hswebframework.ezorm.rdb.utils.ExceptionUtils;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class DefaultSaveOrUpdateOperator implements SaveOrUpdateOperator {

    private RDBTableMetadata table;

    private RDBColumnMetadata idColumn;

    private volatile boolean idColumnParsed;

    public static DefaultSaveOrUpdateOperator of(RDBTableMetadata table) {
        return new DefaultSaveOrUpdateOperator(table);
    }

    public DefaultSaveOrUpdateOperator(RDBTableMetadata table) {
        this.table = table;
    }

    protected RDBColumnMetadata getIdColumn() {
        if (idColumnParsed) {
            return idColumn;
        }
        if (idColumn == null) {
            this.idColumn = table.getColumns()
                    .stream()
                    .filter(RDBColumnMetadata::isPrimaryKey)
                    .findFirst()
                    .orElse(null);
            idColumnParsed = true;
        }

        return idColumn;
    }

    @Getter
    @AllArgsConstructor
    protected static class UpdateOrInsert {
        SqlRequest updateSql;
        Supplier<SqlRequest> insertSql;
    }

    @Override
    public SaveResultOperator execute(UpsertOperatorParameter parameter) {
        return new DefaultSaveResultOperator(() -> createUpsert(parameter));
    }

    protected Upsert createUpsert(UpsertOperatorParameter parameter) {
        Map<String, InsertColumn> mapping = parameter.getColumns().stream()
                .collect(Collectors.toMap(InsertColumn::getColumn, Function.identity()));
        InsertSqlBuilder insertSqlBuilder = table.findFeatureNow(InsertSqlBuilder.ID);
        List<SqlRequest> insert = new ArrayList<>();
        List<UpdateOrInsert> uoi = new ArrayList<>();

        if (getIdColumn() != null) {
            InsertOperatorParameter insertParameter = new InsertOperatorParameter();
            insertParameter.getColumns().addAll(parameter.getColumns());

            InsertColumn id = mapping.getOrDefault(idColumn.getName(), mapping.get(idColumn.getAlias()));
            if (id != null) {
                //update
                UpdateSqlBuilder updateSqlBuilder = table.findFeatureNow(UpdateSqlBuilder.ID);
                Set<UpsertColumn> columns = parameter.getColumns();
                V:
                for (List<Object> value : parameter.getValues()) {
                    UpdateOperatorParameter updateParameter = new UpdateOperatorParameter();
                    int index = 0;
                    for (UpsertColumn column : columns) {
                        if (column.getColumn().equals(id.getColumn())) {
                            Object idValue = value.get(index);
                            if (idValue == null) {//ID未指定则新增
                                insertParameter.getValues().add(value);
                                continue V;
                            }
                            Term whereIdIs = new Term();
                            whereIdIs.setColumn(idColumn.getName());
                            whereIdIs.setValue(idValue);
                            updateParameter.getWhere().add(whereIdIs);
                        } else {
                            if (column.isUpdateIgnore()) {
                                index++;
                                continue;
                            }
                            UpdateColumn updateColumn = new UpdateColumn();
                            updateColumn.setValue(value.get(index));
                            updateColumn.setColumn(column.getColumn());
                            updateColumn.setFunction(column.getFunction());
                            updateParameter.getColumns().add(updateColumn);
                        }
                        index++;
                    }
                    uoi.add(new UpdateOrInsert(updateSqlBuilder.build(updateParameter),
                            () -> {
                                InsertOperatorParameter insertOperatorParameter = new InsertOperatorParameter();
                                insertOperatorParameter.getColumns().addAll(columns);
                                insertOperatorParameter.getValues().add(value);
                                return insertSqlBuilder.build(insertOperatorParameter);
                            }));
                }
            } else {
                insertParameter.getColumns().add(InsertColumn.of(idColumn.getName()));
                insertParameter.getValues().addAll(parameter.getValues());
            }
            if (!insertParameter.getValues().isEmpty()) {
                insert.add(insertSqlBuilder.build(insertParameter));
            }
        } else {
            InsertOperatorParameter insertParameter = new InsertOperatorParameter();
            insertParameter.setColumns(parameter.toInsertColumns());
            insertParameter.setValues(parameter.getValues());
            insert.add(insertSqlBuilder.build(insertParameter));
        }
        return new Upsert(insert, uoi);
    }

    @AllArgsConstructor
    static class Upsert {
        protected List<SqlRequest> insert;
        protected List<UpdateOrInsert> upserts;
    }


    @AllArgsConstructor
    protected class DefaultSaveResultOperator implements SaveResultOperator {

        Supplier<Upsert> supplier;

        @Override
        public SaveResult sync() {
            SyncSqlExecutor sqlExecutor = table.findFeatureNow(SyncSqlExecutor.ID);
            return ExceptionUtils.translation(() -> {
                Upsert upsert = supplier.get();
                int inserted = 0;
                int updated = 0;

                for (SqlRequest sqlRequest : upsert.insert) {
                    inserted += sqlExecutor.update(sqlRequest);
                }
                for (UpdateOrInsert updateOrInsert : upsert.upserts) {
                    int tmp = sqlExecutor.update(updateOrInsert.updateSql);
                    updated += tmp;
                    if (tmp == 0) {
                        inserted += sqlExecutor.update(updateOrInsert.insertSql.get());
                    }
                }
                return SaveResult.of(inserted, updated);
            }, table);
        }

        @Override
        public Mono<SaveResult> reactive() {

            return Mono.defer(() -> {
                ReactiveSqlExecutor sqlExecutor = table.findFeatureNow(ReactiveSqlExecutor.ID);
                Upsert upsert = supplier.get();
                return sqlExecutor
                        .update(Flux.fromIterable(upsert.insert))
                        .flatMap(inserted ->
                                Flux.fromIterable(upsert.upserts)
                                        .flatMap(updateOrInsert ->
                                                sqlExecutor.update(Mono.just(updateOrInsert.updateSql))
                                                        .flatMap(updated -> {
                                                            if (updated == 0) {
                                                                return sqlExecutor
                                                                        .update(Mono.just(updateOrInsert.insertSql.get()))
                                                                        .map(r -> SaveResult.of(r, 0));
                                                            }
                                                            return Mono.just(SaveResult.of(0, updated));
                                                        })).reduce(SaveResult.of(inserted, 0), SaveResult::merge))
                        .as(ExceptionUtils.translation(table));

            });
        }
    }
}
