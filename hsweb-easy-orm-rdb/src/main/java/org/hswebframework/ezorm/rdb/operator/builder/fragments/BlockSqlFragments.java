package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.NoArgsConstructor;
import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;

import java.util.*;
import java.util.stream.Collectors;

@NoArgsConstructor(staticName = "of")
public class BlockSqlFragments implements SqlFragments {

    private final Map<FragmentBlock, LinkedList<SqlFragments>> blocks = new TreeMap<>(FragmentBlock::compareTo);

    @Override
    public boolean isEmpty() {
        return blocks.isEmpty();
    }

    @Override
    public List<String> getSql() {

        List<String> sql = new ArrayList<>(blocks.size() * 5);

        for (LinkedList<SqlFragments> value : blocks.values()) {
            for (SqlFragments fragments : value) {
                sql.addAll(fragments.getSql());
            }
        }

        return sql;
    }

    @Override
    public List<Object> getParameters() {
        List<Object> sql = new ArrayList<>(blocks.size() * 5);

        for (LinkedList<SqlFragments> value : blocks.values()) {
            for (SqlFragments fragments : value) {
                sql.addAll(fragments.getParameters());
            }
        }

        return sql;

    }


    public BlockSqlFragments addBlock(FragmentBlock block, String sql) {
        getBlock(block).add(SqlFragments.single(sql));
        return this;
    }

    public BlockSqlFragments addBlockFirst(FragmentBlock block, String sql) {
        getBlock(block).addFirst(SqlFragments.single(sql));
        return this;
    }

    public BlockSqlFragments addBlock(FragmentBlock block, SqlFragments fragments) {
        getBlock(block).add(fragments);
        return this;
    }

    public BlockSqlFragments addBlockFirst(FragmentBlock block, SqlFragments fragments) {
        getBlock(block).addFirst(fragments);
        return this;
    }


    public LinkedList<SqlFragments> getBlock(FragmentBlock block) {
        return blocks.computeIfAbsent(block, __ -> new LinkedList<>());
    }

    @Override
    public String toString() {

        return toRequest().toString();
    }
}
