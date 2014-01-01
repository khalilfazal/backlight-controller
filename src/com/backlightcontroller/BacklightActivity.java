package com.backlightcontroller;

import java.net.URI;
import java.util.Set;
import java.util.TreeSet;

import org.apache.http.HttpHost;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.WindowManager;
import android.view.inputmethod.InputMethodManager;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.AutoCompleteTextView.OnDismissListener;
import android.widget.SeekBar;
import android.widget.SeekBar.OnSeekBarChangeListener;

/**
 * The Class BacklightActivity.
 */
public class BacklightActivity extends Activity {

    /**
     * The Constant PREFS_NAME.
     */
    private static final String PREFS_NAME = "Backlight Controller";

    /**
     * The Constant brightness.
     */
    private static final String _brightness = "brightness";

    /**
     * The Constant hostname.
     */
    private static final String _hostname = "hostname";

    /**
     * The Constant _hostnames.
     */
    private static final String _hostnames = "hostnames";

    /**
     * The seek bar.
     */
    private SeekBar seekBar;

    /**
     * The max.
     */
    private int max;

    /**
     * The hostname.
     */
    private String hostname;

    /**
     * The preferences.
     */
    protected SharedPreferences preferences;

    /**
     * The hostnames.
     */
    private Set<String> hostnames;

    /**
     * On create.
     *
     * @param savedInstanceState the saved instance state
     * @see android.app.Activity#onCreate(android.os.Bundle)
     */
    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        this.setContentView(R.layout.main);
        this.preferences = this.getSharedPreferences(PREFS_NAME, 0);
        this.seekBar = (SeekBar) this.findViewById(R.id.seekBar);
        this.max = this.seekBar.getMax();
    }

    /**
     * On resume.
     *
     * @see android.app.Activity#onResume() 
     */
    @Override
    protected void onResume() {
        super.onResume();
        this.seekBar.setProgress(this.preferences.getInt(_brightness, this.max));
        this.hostnames = new TreeSet<String>(this.preferences.getStringSet(_hostnames, new TreeSet<String>()));
        this.addListener(this.preferences.getString(_hostname, ""));
    }

    /**
     * Adds the listener.
     *
     * @param hostname the hostname
     */
    protected void addListener(final String hostname) {
        if (this.validHostname(hostname)) {
            this.hostnames.add(this.hostname = hostname);
            final OnSeekBarChangeListener listener = new BacklightAdjuster(this.hostname, this.max);
            this.seekBar.setOnSeekBarChangeListener(listener);
        } else {
            final AutoCompleteTextView input = (AutoCompleteTextView) this.getLayoutInflater().inflate(R.layout.input_hostname, null);
            final String[] aHostnames = this.hostnames.toArray(new String[this.hostnames.size()]);
            final ArrayAdapter<String> adapter = new ArrayAdapter<String>(this, android.R.layout.simple_dropdown_item_1line, aHostnames);
            input.setAdapter(adapter);

            final InputMethodManager imm = (InputMethodManager) this.getSystemService(Context.INPUT_METHOD_SERVICE);

            input.setOnTouchListener(new View.OnTouchListener() {
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

            final DialogInterface.OnClickListener listener = new DialogInterface.OnClickListener() {
                @Override
                public void onClick(final DialogInterface dialog, final int which) {
                    BacklightActivity.this.addListener(input.getText().toString());
                }
            };

            final AlertDialog dialog = new AlertDialog.Builder(this)
                    .setTitle(R.string.hostnameTitle)
                    .setMessage(R.string.hostnameMessage)
                    .setView(input)
                    .setNeutralButton(R.string.hostnameSet, listener)
                    .create();

            input.setOnFocusChangeListener(new View.OnFocusChangeListener() {
                @Override
                public void onFocusChange(final View v, final boolean hasFocus) {
                    if (hasFocus) {
                        dialog.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_VISIBLE);
                    }
                }
            });

            dialog.show();
        }
    }

    /**
     * Valid hostname.
     *
     * @param hostname the hostname
     * @return true, if successful
     */
    @SuppressWarnings("unused")
    private boolean validHostname(final String hostname) {
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
     * On pause.
     *
     * @see android.app.Activity#onPause()
     */
    @Override
    protected void onPause() {
        super.onPause();

        final Editor editor = this.preferences.edit();
        editor.putInt(_brightness, this.seekBar.getProgress());
        editor.putString(_hostname, this.hostname);
        editor.putStringSet(_hostnames, this.hostnames);
        editor.commit();
    }

    /**
     * On create options menu.
     *
     * @param menu the menu
     * @return true, if successful
     * @see android.app.Activity#onCreateOptionsMenu(android.view.Menu)
     */
    @Override
    public boolean onCreateOptionsMenu(final Menu menu) {
        this.getMenuInflater().inflate(R.menu.menu, menu);
        return super.onCreateOptionsMenu(menu);
    }

    /**
     * On options item selected.
     *
     * @param item the item
     * @return true, if successful
     * @see android.app.Activity#onOptionsItemSelected(android.view.MenuItem)
     */
    @Override
    public boolean onOptionsItemSelected(final MenuItem item) {
        switch (item.getItemId()) {
            case R.id.hostname:
                this.addListener("");
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }
}