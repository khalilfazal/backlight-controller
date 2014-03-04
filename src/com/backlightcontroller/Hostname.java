package com.backlightcontroller;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.apache.http.HttpHost;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.content.SharedPreferences;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnFocusChangeListener;
import android.view.View.OnTouchListener;
import android.view.WindowManager;
import android.view.inputmethod.InputMethodManager;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.AutoCompleteTextView.OnDismissListener;

/**
 * The Class Hostname.
 *
 * @author Khalil Fazal
 */
public abstract class Hostname {

    /**
     * The Constant _hostnames.
     */
    public static final String _hostnames = "hostnames";

    /**
     * The Constant hostname.
     */
    public static final String _hostname = "hostname";

    /**
     * Valid hostname.
     * @param hostname the hostname to test
     *
     * @return true, if successful
     */
    @SuppressWarnings("unused")
    private static boolean valid(final String hostname) {
        try {
            final URI requestURI = new URI("http://" + hostname);
            new HttpHost(
                    requestURI.getHost(),
                    requestURI.getPort(),
                    requestURI.getScheme());
        } catch (final Exception e) {
            return false;
        }

        return true;
    }

    /**
     * The hostname.
     */
    private String hostname;

    /**
     * The act.
     */
    private final Activity act;

    /**
     * The hostnames.
     */
    private final Set<String> hostnames;

    /**
     * Instantiates a new hostname.
     *
     * @param act the activity in whose context dialogs will be shown
     * @param preferences from where saved hostnames and the last valid hostname is stored
     */
    public Hostname(final Activity act, final SharedPreferences preferences) {
        this.act = act;
        this.hostnames = new TreeSet<String>(preferences.getStringSet(_hostnames, Collections.<String> emptySet()));
        this.get(preferences.getString(_hostname, ""));
    }

    /**
     * Call.
     */
    public abstract void callback();

    /**
     * Adds the listener.
     * @param hostname initial hostname
     */
    public void get(final String hostname) {
        if (valid(hostname)) {
            this.hostnames.add(this.hostname = hostname);
            this.callback();
        } else {
            this.prompt();
        }
    }

    /**
     * Gets the hostname.
     */
    private void prompt() {
        final AutoCompleteTextView input = this.input();
        final List<String> hostnameList = new ArrayList<String>(this.hostnames);
        final ArrayAdapter<String> adapter = new ArrayAdapter<String>(this.act, android.R.layout.simple_dropdown_item_1line, hostnameList);
        input.setAdapter(adapter);

        final OnClickListener listener = new OnClickListener() {
            @Override
            public void onClick(final DialogInterface dialog, final int which) {
                Hostname.this.get(input.getText().toString());
            }
        };

        final AlertDialog dialog = new AlertDialog.Builder(this.act)
                .setTitle(R.string.hostnameTitle)
                .setMessage(R.string.hostnameMessage)
                .setView(input)
                .setNeutralButton(R.string.hostnameSet, listener)
                .create();

        input.setOnFocusChangeListener(new OnFocusChangeListener() {
            @Override
            public void onFocusChange(final View v, final boolean hasFocus) {
                if (hasFocus) {
                    dialog.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_VISIBLE);
                }
            }
        });

        dialog.show();
    }

    /**
     * Gets the text field.
     *
     * @return the text field
     */
    private AutoCompleteTextView input() {
        final AutoCompleteTextView input = (AutoCompleteTextView) this.act.getLayoutInflater().inflate(R.layout.input_hostname, null);
        final InputMethodManager imm = (InputMethodManager) this.act.getSystemService(Context.INPUT_METHOD_SERVICE);

        input.setOnTouchListener(new OnTouchListener() {
            @Override
            public boolean onTouch(final View v, final MotionEvent event) {
                imm.hideSoftInputFromWindow(v.getWindowToken(), 0);
                input.showDropDown();
                return true;
            }
        });

        input.setOnDismissListener(new OnDismissListener() {
            @Override
            public void onDismiss() {
                imm.showSoftInput(input, 0);
            }
        });

        return input;
    }

    /**
     * To string.
     *
     * @return the string
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.hostname;
    }
}
