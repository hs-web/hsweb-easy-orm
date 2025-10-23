package org.hswebframework.ezorm.rdb.codec;

import org.reactivestreams.Publisher;
import reactor.util.annotation.NonNull;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.stream.IntStream;

public class ClobValue implements CharSequence {

    private final CharSequence charSequence;

    public ClobValue(CharSequence charSequence) {
        this.charSequence = charSequence;
    }

    public CharSequence source() {
        return charSequence;
    }

    public Reader reader() {
        return new StringReader(String.valueOf(charSequence));
    }

    @Override
    public String toString() {
        return charSequence.toString();
    }

    @Override
    public int length() {
        return charSequence.length();
    }

    @Override
    public char charAt(int index) {
        return charSequence.charAt(index);
    }

    @Override
    public boolean isEmpty() {
        return charSequence.isEmpty();
    }

    @Override
    @NonNull
    public CharSequence subSequence(int start, int end) {
        return charSequence.subSequence(start, end);
    }

    @Override
    @NonNull
    public IntStream chars() {
        return charSequence.chars();
    }

    @Override
    @NonNull
    public IntStream codePoints() {
        return charSequence.codePoints();
    }
}
