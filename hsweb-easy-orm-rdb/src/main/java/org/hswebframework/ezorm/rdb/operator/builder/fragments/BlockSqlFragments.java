package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.NoArgsConstructor;
import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;
import org.hswebframework.ezorm.rdb.utils.FlatList;

import java.util.*;

@NoArgsConstructor(staticName = "of")
public class BlockSqlFragments implements SqlFragments {

    private final Map<FragmentBlock, LinkedList<SqlFragments>> blocks = new TreeMap<>(FragmentBlock::compareTo);

    @Override
    public boolean isEmpty() {
        return blocks.isEmpty();
    }

    @Override
    public List<String> getSql() {
        List<List<String>> sql = new ArrayList<>(blocks.size());

        for (LinkedList<SqlFragments> value : blocks.values()) {
            for (SqlFragments fragments : value) {
                sql.add(fragments.getSql());
            }
        }

        return new FlatList<>(sql);
    }

    @Override
    public List<Object> getParameters() {
        List<List<Object>> params = new ArrayList<>(blocks.size());

        for (LinkedList<SqlFragments> value : blocks.values()) {
            for (SqlFragments fragments : value) {
                params.add(fragments.getParameters());
            }
        }
        return new FlatList<>(params);
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
