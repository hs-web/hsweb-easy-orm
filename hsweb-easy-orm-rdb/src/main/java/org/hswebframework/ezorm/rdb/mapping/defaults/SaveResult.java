package org.hswebframework.ezorm.rdb.mapping.defaults;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor(staticName = "of")
public class SaveResult {
    private int added;

    private int updated;

    public int getTotal() {
        return added + updated;
    }

    public SaveResult merge(SaveResult result) {
        SaveResult res = SaveResult.of(added, updated);

        res.added += result.getAdded();
        res.updated += result.getUpdated();

        return res;
    }

    @Override
    public String toString() {
        return "added " + added + ",updated " + updated + ",total " + getTotal();
    }
}
